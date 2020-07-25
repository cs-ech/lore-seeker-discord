#![deny(rust_2018_idioms, unused, unused_import_braces, unused_qualifications, warnings)]

use {
    std::{
        env,
        fmt,
        io::{
            self,
            prelude::*
        },
        path::PathBuf,
        sync::{
            PoisonError,
            RwLockReadGuard
        },
        time::Duration
    },
    derive_more::From,
    kuchiki::traits::TendrilSink as _,
    serenity::model::prelude::*,
    url::Url
};

#[derive(Debug, From)]
pub enum Error {
    #[from(ignore)]
    Annotated(String, Box<Error>),
    ChannelIdParse(ChannelIdParseError),
    Db(mtg::card::DbError),
    DmOnlyCommand,
    Env(env::VarError),
    #[from(ignore)]
    Io(io::Error, Option<PathBuf>),
    Json(serde_json::Error),
    MissingANode,
    MissingCardDb,
    MissingCardLink,
    MissingCardList,
    MissingContext,
    MissingHref,
    MissingInlineChannels,
    MissingNewline,
    MissingOwners,
    MissingTextNode,
    MomirMissingCmc,
    #[from(ignore)]
    NoSuchCard(String),
    OwnerCheck,
    ParseInt(std::num::ParseIntError),
    Poison,
    Reqwest(reqwest::Error),
    Serenity(serenity::Error),
    Shlex,
    #[from(ignore)]
    UnknownCommand(String),
    UrlParse(url::ParseError)
}

impl<'a> From<PoisonError<RwLockReadGuard<'a, serenity::cache::Cache>>> for Error {
    fn from(_: PoisonError<RwLockReadGuard<'a, serenity::cache::Cache>>) -> Error {
        Error::Poison
    }
}

pub trait IntoResultExt {
    type T;

    fn annotate(self, note: impl ToString) -> Self::T;
}

impl<E: Into<Error>> IntoResultExt for E {
    type T = Error;

    fn annotate(self, note: impl ToString) -> Error {
        Error::Annotated(note.to_string(), Box::new(self.into()))
    }
}

impl IntoResultExt for io::Error {
    type T = Error;

    fn annotate(self, note: impl ToString) -> Error {
        Error::Annotated(note.to_string(), Box::new(self.at_unknown()))
    }
}

impl<T, E: IntoResultExt> IntoResultExt for Result<T, E> {
    type T = Result<T, E::T>;

    fn annotate(self, note: impl ToString) -> Result<T, E::T> {
        self.map_err(|e| e.annotate(note))
    }
}

pub trait IoResultExt {
    type T;

    fn at(self, path: impl AsRef<std::path::Path>) -> Self::T;
    fn at_unknown(self) -> Self::T;
}

impl IoResultExt for io::Error {
    type T = Error;

    fn at(self, path: impl AsRef<std::path::Path>) -> Error {
        Error::Io(self, Some(path.as_ref().to_owned()))
    }

    fn at_unknown(self) -> Error {
        Error::Io(self, None)
    }
}

impl<T, E: IoResultExt> IoResultExt for Result<T, E> {
    type T = Result<T, E::T>;

    fn at(self, path: impl AsRef<std::path::Path>) -> Result<T, E::T> {
        self.map_err(|e| e.at(path))
    }

    fn at_unknown(self) -> Result<T, E::T> {
        self.map_err(|e| e.at_unknown())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Annotated(note, e) => write!(f, "{}: {}", note, e),
            Error::ChannelIdParse(e) => e.fmt(f),
            Error::Db(e) => write!(f, "card database error: {:?}", e), //TODO implement Display for DbError
            Error::DmOnlyCommand => write!(f, "to avoid spamming channels, this command is only allowed in direct messages"),
            Error::Env(e) => e.fmt(f),
            Error::Io(e, Some(path)) => write!(f, "I/O error at {}: {}", path.display(), e),
            Error::Io(e, None) => write!(f, "I/O error: {}", e),
            Error::Json(e) => write!(f, "JSON error: {}", e),
            Error::MissingANode => write!(f, "failed to parse Lore Seeker search results: missing a node"),
            Error::MissingCardDb => write!(f, "attempted to access card database before loading it"),
            Error::MissingCardLink => write!(f, "failed to parse Lore Seeker search results: missing card link"),
            Error::MissingCardList => write!(f, "failed to parse Lore Seeker search results: missing card list"),
            Error::MissingContext => write!(f, "Serenity context not available before ready event"),
            Error::MissingHref => write!(f, "failed to parse Lore Seeker search results: missing href"),
            Error::MissingInlineChannels => write!(f, "failed to load inline channels config"),
            Error::MissingNewline => write!(f, "IPC thread closed without trailing newline"),
            Error::MissingOwners => write!(f, "failed to load list of bot owners"),
            Error::MissingTextNode => write!(f, "failed to parse Lore Seeker search results: missing text node"),
            Error::MomirMissingCmc => write!(f, "missing CMC for Momir Basic"),
            Error::NoSuchCard(card_name) => write!(f, "no such card: {}", card_name),
            Error::OwnerCheck => write!(f, "this command can only be used by the bot owner"),
            Error::ParseInt(e) => write!(f, "invalid number: {}", e),
            Error::Poison => write!(f, "poison error"),
            Error::Reqwest(e) => if let Some(url) = e.url() {
                write!(f, "reqwest error at {}: {}", url, e)
            } else {
                write!(f, "reqwest error: {}", e)
            },
            Error::Serenity(e) => e.fmt(f),
            Error::Shlex => write!(f, "failed to parse command line"),
            Error::UnknownCommand(cmd) => write!(f, "unknown command: {}", cmd),
            Error::UrlParse(e) => e.fmt(f),
        }
    }
}

/// Requests the page at the given path (which must include the initial `/`) from Lore Seeker
///
/// # Features
///
/// If the `local` feature is enabled (it is by default), this function will use the hostname `localhost:18803`.
/// Otherwise, the return value of `hostname` is used.
pub fn get(path: String) -> Result<reqwest::Response, Error> {
    #[cfg(feature = "local")] let hostname = if is_dev() { "localhost:18808" } else { "localhost:18803" };
    #[cfg(not(feature = "local"))] let hostname = hostname();
    Ok(
        reqwest::ClientBuilder::new()
            .timeout(Some(Duration::from_secs(60))) // increased timeout due to performance issues in %sealed
            .build()?
            .get(&format!("http://{}{}", hostname, path))
            .send()?
            .error_for_status()?
    )
}

pub fn hostname() -> &'static str {
    if is_dev() {
        "dev.lore-seeker.cards"
    } else {
        "lore-seeker.cards"
    }
}

pub fn is_dev() -> bool {
    env::var_os("LORESEEKERDATA") == Some("/usr/local/share/fenhl/lore-seeker/dev".into())
}

/// Sends the given query to Lore Seeker using `get` and returns the URL-encoded query as well as the list of (card name, card page) pairs.
pub fn resolve_query(host: Option<&str>, query: &str) -> Result<(String, Vec<(String, Url)>), Error> {
    let encoded_query = urlencoding::encode(if query.is_empty() { "*" } else { query });
    let document = {
        let mut response = get(format!("/list?q={}", encoded_query))?;
        let mut response_content = String::default();
        response.read_to_string(&mut response_content).annotate("resolve_query")?;
        kuchiki::parse_html().one(response_content)
    };
    let card_list_data = document.select_first("ul#search-result").map_err(|()| Error::MissingCardList)?;
    let card_list = card_list_data.as_node();
    let mut matches = Vec::default();
    let base_url = Url::parse(&format!("https://{}/", host.unwrap_or_else(|| hostname())))?;
    for li_node_data in card_list.select("li").map_err(|()| Error::MissingCardList)? {
        let li_node = li_node_data.as_node();
        let a_node_data = li_node.select_first("a").map_err(|()| Error::MissingCardLink)?;
        let a_node = a_node_data.as_node();
        let a_elt = a_node.as_element().ok_or(Error::MissingANode)?;
        let text_node = a_node.first_child().ok_or(Error::MissingTextNode)?;
        let href = a_elt.attributes.borrow().get("href").ok_or(Error::MissingHref).and_then(|href| base_url.join(href).map_err(Error::from))?;
        let text = text_node.as_text().ok_or(Error::MissingTextNode)?;
        matches.push((text.borrow().trim().to_owned(), href));
    }
    Ok((encoded_query, matches))
}
