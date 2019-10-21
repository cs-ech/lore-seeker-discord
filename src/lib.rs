#![deny(rust_2018_idioms, unused, unused_import_braces, unused_qualifications, warnings)]

use {
    std::{
        env,
        io::{
            self,
            prelude::*
        },
        sync::{
            PoisonError,
            RwLockReadGuard
        },
        time::Duration
    },
    derive_more::From,
    kuchiki::traits::TendrilSink as _,
    url::Url
};

pub const HOSTNAME: &str = "lore-seeker.cards";

#[derive(Debug, From)]
pub enum Error {
    Db(mtg::card::DbError),
    DmOnlyCommand,
    Env(env::VarError),
    Io(io::Error),
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
    NoSuchCard(String),
    OwnerCheck,
    ParseInt(std::num::ParseIntError),
    Poison,
    Reqwest(reqwest::Error),
    Serenity(serenity::Error),
    Shlex,
    UnknownCommand(String),
    UrlParse(url::ParseError)
}

impl<'a> From<PoisonError<RwLockReadGuard<'a, serenity::cache::Cache>>> for Error {
    fn from(_: PoisonError<RwLockReadGuard<'a, serenity::cache::Cache>>) -> Error {
        Error::Poison
    }
}

/// Requests the page at the given path (which must include the initial `/`) from Lore Seeker
///
/// # Features
///
/// If the `local` feature is enabled (it is by default), this function will use the hostname `localhost:18803`.
/// Otherwise, the value of the constant `HOSTNAME` is used.
pub fn get(path: String) -> Result<reqwest::Response, Error> {
    #[cfg(feature = "local")] let hostname = "localhost:18803";
    #[cfg(not(feature = "local"))] let hostname = HOSTNAME;
    Ok(
        reqwest::ClientBuilder::new()
            .timeout(Some(Duration::from_secs(60))) // increased timeout due to performance issues in %sealed
            .build()?
            .get(&format!("http://{}{}", hostname, path))
            .send()?
            .error_for_status()?
    )
}

/// Sends the given query to Lore Seeker using `get` and returns the URL-encoded query as well as the list of (card name, card page) pairs.
pub fn resolve_query(query: &str) -> Result<(String, Vec<(String, Url)>), Error> {
    let encoded_query = urlencoding::encode(if query.is_empty() { "*" } else { query });
    let document = {
        let mut response = get(format!("/list?q={}", encoded_query))?;
        let mut response_content = String::default();
        response.read_to_string(&mut response_content)?;
        kuchiki::parse_html().one(response_content)
    };
    let card_list_data = document.select_first("ul#search-result").map_err(|()| Error::MissingCardList)?;
    let card_list = card_list_data.as_node();
    let mut matches = Vec::default();
    let base_url = Url::parse(&format!("https://{}/", HOSTNAME))?;
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
