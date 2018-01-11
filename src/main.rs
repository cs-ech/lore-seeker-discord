#![warn(trivial_casts)]
#![deny(unused)]
#![forbid(unused_import_braces)]

extern crate chrono;
extern crate kuchiki;
extern crate mtg;
extern crate reqwest;
extern crate serenity;
extern crate typemap;
extern crate urlencoding;

use std::{env, io, process};
use std::collections::HashSet;
use std::io::prelude::*;
use std::sync::{PoisonError, RwLockReadGuard};

use chrono::prelude::*;

use kuchiki::traits::TendrilSink;

use mtg::card::Db;

use serenity::builder::CreateEmbedField;
use serenity::prelude::*;
use serenity::model::{EmojiId, Guild, Message, Permissions, ReactionType, Ready, UserId};
use serenity::utils::MessageBuilder;

use typemap::Key;

macro_rules! manamoji {
    ($($sym:expr => $name:expr, $id:expr;)+) => {
        #[allow(unused)] //TODO
        fn manamoji(s: &str) -> Option<ReactionType> {
            match s {
                $($sym => Some(ReactionType::Custom {
                    id: EmojiId($id),
                    name: Some($name.to_owned()),
                    animated: false
                }),)+
                _ => None
            }
        }

        fn with_manamoji(s: &str) -> String {
            $(
                let mut split = s.split($sym);
                let mut s = split.next().expect("failed to convert manamoji").to_owned();
                for part in split {
                    s.push_str(&ReactionType::Custom {
                        id: EmojiId($id),
                        name: Some($name.to_owned()),
                        animated: false
                    }.to_string());
                    s.push_str(part);
                }
            )+
            s
        }
    };
}

manamoji! {
    "{0}" => "mana0", 386740600436686878;
    "{1}" => "mana1", 386740600399069204;
    "{10}" => "mana10", 386740603708506142;
    "{11}" => "mana11", 386741116927475718;
    "{12}" => "mana12", 386741117577592833;
    "{13}" => "mana13", 386741118303469570;
    "{14}" => "mana14", 386741118668242964;
    "{15}" => "mana15", 386741118273847307;
    "{16}" => "mana16", 386741118597070850;
    "{17}" => "mana17", 386741120434044948;
    "{18}" => "mana18", 386741120006094860;
    "{19}" => "mana19", 386741120371261449;
    "{2}" => "mana2", 386740600755453953;
    "{2/B}" => "mana2b", 386741115807596547;
    "{2/G}" => "mana2g", 386741116118106122;
    "{2/R}" => "mana2r", 386741116222963712;
    "{2/U}" => "mana2u", 386741115933687809;
    "{2/W}" => "mana2w", 386741116462039043;
    "{20}" => "mana20", 386741120543227906;
    "{3}" => "mana3", 386740601082871822;
    "{4}" => "mana4", 386740601556828160;
    "{5}" => "mana5", 386740601724338176;
    "{6}" => "mana6", 386740602265403392;
    "{7}" => "mana7", 386740602374717440;
    "{8}" => "mana8", 386740602747879435;
    "{9}" => "mana9", 386740603494334484;
    "{B}" => "manab", 386740604341583873;
    "{B/G}" => "manabg", 386740605847470081;
    "{B/P}" => "manabp", 386741121486815232;
    "{B/R}" => "manabr", 386740607017549838;
    "{C}" => "manac", 386740607416139777;
    "{CHAOS}" => "manachaos", 386741121608318986;
    "{E}" => "manae", 386741121780416513;
    "{G}" => "manag", 386740607827181569;
    "{G/P}" => "managp", 386741122225012761;
    "{G/U}" => "managu", 386740607906873344;
    "{G/W}" => "managw", 386740607994953728;
    "{Q}" => "manaq", 386741125987434506;
    "{R}" => "manar", 386740612394778634;
    "{R/G}" => "manarg", 386740613430640640;
    "{R/P}" => "manarp", 386741125773262860;
    "{R/W}" => "manarw", 386740615498563588;
    "{S}" => "manas", 386741126939541505;
    "{T}" => "manat", 386740612143120385;
    "{U}" => "manau", 386740612289789953;
    "{U/B}" => "manaub", 386740615683112992;
    "{U/P}" => "manaup", 386741126247350284;
    "{U/R}" => "manaur", 386740615649558534;
    "{W}" => "manaw", 386740617792978944;
    "{W/B}" => "manawb", 386740617780264960;
    "{W/P}" => "manawp", 386741126645809162;
    "{W/U}" => "manawu", 386740617792978964;
    "{X}" => "manax", 386740617667018752;
    "{Y}" => "manay", 386741126457065473;
    "{Z}" => "manaz", 386741127207845890;
    "{hr}" => "manahr", 386741125672730634;
    "{hw}" => "manahw", 386741125773262848;
    "{½}" => "manahalf", 386741122510225421;
    "{∞}" => "manainfinity", 386741125861605387;
}

struct CardDb;

impl Key for CardDb {
    type Value = Db;
}

struct Owners;

impl Key for Owners {
    type Value = HashSet<UserId>;
}

struct Handler;

impl EventHandler for Handler {
    fn on_ready(&self, ctx: Context, ready: Ready) {
        let guilds = ready.user.guilds().expect("failed to get guilds");
        if guilds.is_empty() {
            println!("[!!!!] No guilds found, use following URL to invite the bot:");
            println!("[ ** ] {}", ready.user.invite_url(Permissions::READ_MESSAGES | Permissions::SEND_MESSAGES | Permissions::USE_EXTERNAL_EMOJIS).expect("failed to generate invite URL"));
            ctx.quit().expect("failed to quit");
            process::exit(1); //TODO (serenity 0.5.0) remove
        }
    }

    fn on_guild_create(&self, _: Context, guild: Guild, _: bool) {
        println!("[ ** ] Connected to {}", guild.name);
    }

    fn on_message(&self, ctx: Context, msg: Message) {
        if msg.author.bot { return; } // ignore bots to prevent message loops
        if let Some(err_reply) = match handle_message(ctx, &msg) {
            Ok(()) => None,
            Err(Error::NoSuchCard(card_name)) => Some(MessageBuilder::default().push("error: no such card: ").push_safe(card_name).build()),
            Err(Error::OwnerCheck) => Some("this command can only be used by the bot owner".into()),
            Err(Error::Reqwest(e)) => {
                println!("{}: Message handler returned reqwest error {:?}", Utc::now().format("%Y-%m-%d %H:%M:%S"), e);
                Some("failed to connect to Lore Seeker website, try again later".into()) //TODO check if lore-seeker screen is running
            }
            Err(Error::UnknownCommand(cmd)) => Some(MessageBuilder::default().push("unknown command: %").push_safe(cmd).build()),
            Err(e) => {
                println!("{}: Message handler returned error {:?}", Utc::now().format("%Y-%m-%d %H:%M:%S"), e);
                Some("unknown error (cc <@86841168427495424>)".into())
            }
        } {
            msg.reply(&err_reply).expect("failed to send error reply");
        }
    }
}

#[derive(Debug)]
enum Error<'a> {
    CssSelector,
    Db(mtg::card::DbError),
    Io(io::Error),
    MissingCardDb,
    MissingCardList,
    MissingOwners,
    NoSuchCard(String),
    OwnerCheck,
    Poison(PoisonError<RwLockReadGuard<'a, serenity::cache::Cache>>),
    Reqwest(reqwest::Error),
    ResponseStatus(reqwest::StatusCode),
    Serenity(serenity::Error),
    UnknownCommand(String)
}

impl<'a> From<PoisonError<RwLockReadGuard<'a, serenity::cache::Cache>>> for Error<'a> {
    fn from(e: PoisonError<RwLockReadGuard<'a, serenity::cache::Cache>>) -> Error<'a> {
        Error::Poison(e)
    }
}

impl<'a> From<io::Error> for Error<'a> {
    fn from(e: io::Error) -> Error<'a> {
        Error::Io(e)
    }
}

impl<'a> From<mtg::card::DbError> for Error<'a> {
    fn from(e: mtg::card::DbError) -> Error<'a> {
        Error::Db(e)
    }
}

impl<'a> From<reqwest::Error> for Error<'a> {
    fn from(e: reqwest::Error) -> Error<'a> {
        Error::Reqwest(e)
    }
}

impl<'a> From<serenity::Error> for Error<'a> {
    fn from(e: serenity::Error) -> Error<'a> {
        Error::Serenity(e)
    }
}

fn eat_word(subj: &mut &str) -> Option<String> {
    if let Some(word) = next_word(*subj) {
        *subj = &subj[word.len()..];
        while subj.starts_with(' ') { *subj = &subj[1..]; }
        Some(word)
    } else {
        None
    }
}

fn handle_message(ctx: Context, msg: &Message) -> Result<(), Error> {
    let current_user_id = serenity::CACHE.read()?.user.id;
    let mut query = if msg.content.starts_with(&current_user_id.mention()) { //TODO allow <@!id> mentions
        let mut query = &msg.content[current_user_id.mention().len()..];
        if query.starts_with(':') { query = &query[1..]; }
        if query.starts_with(' ') { query = &query[1..]; }
        Some(query)
    } else if msg.author.create_dm_channel().ok().map_or(false, |dm| dm.id == msg.channel_id) {
        Some(&msg.content[..])
    } else {
        None
    };
    if query.map_or(false, |q| q.starts_with('%')) {
        // command
        query = Some(&query.unwrap()[1..]);
        if let Some(cmd_name) = eat_word(&mut query.unwrap()) {
            match &cmd_name[..] {
                "quit" => {
                    owner_check(&ctx, &msg)?;
                    ctx.quit()?;
                    return Ok(());
                }
                "reload" | "update" => {
                    owner_check(&ctx, &msg)?;
                    let mut data = ctx.data.lock();
                    let db = Db::from_sets_dir("/opt/git/github.com/fenhl/lore-seeker/stage/data/sets")?;
                    data.insert::<CardDb>(db);
                    msg.react("✅")?;
                    return Ok(());
                }
                cmd => {
                    return Err(Error::UnknownCommand(cmd.into()));
                }
            }
        }
    }
    if let Some(query) = query {
        let encoded_query = urlencoding::encode(query);
        let mut response = reqwest::get(&format!("http://localhost:18803/list?q={}", encoded_query))?;
        if !response.status().is_success() {
            return Err(Error::ResponseStatus(response.status()));
        }
        let mut response_content = String::default();
        response.read_to_string(&mut response_content)?;
        let document = kuchiki::parse_html().one(response_content);
        let mut matches = document.select("ul#search-result").map_err(|()| Error::CssSelector)?
            .next().ok_or(Error::MissingCardList)?
            .as_node()
            .children()
            .filter_map(|node| node.first_child().and_then(|text_node| text_node.as_text().map(|text| text.borrow().trim().to_owned())));
        match (matches.next(), matches.next()) {
            (Some(_), Some(_)) => { msg.reply(&format!("{} cards found: https://loreseeker.fenhl.net/card?q={}", 2 + matches.count(), encoded_query))?; }
            (Some(card_name), None) => {
                let card_url = format!("https://loreseeker.fenhl.net/card?q=!{}", urlencoding::encode(&card_name)); //TODO use exact printing URL
                let mut reply = msg.reply(&format!("1 card found: {}", card_url))?;
                let card = {
                    let data = ctx.data.lock();
                    let db = data.get::<CardDb>().ok_or(Error::MissingCardDb)?;
                    db.card(&card_name).ok_or(Error::NoSuchCard(card_name.clone()))?
                };
                reply.edit(|m| m
                    .embed(|e| e
                        .color(match card.rarity().color() {
                            (0, 0, 0) => (1, 1, 1), // Discord turns actual black into light gray
                            c => c
                        })
                        .title(format!("{} {}", card, with_manamoji(&card.mana_cost().map_or("".to_owned(), |cost| cost.to_string()))))
                        .url(&card_url)
                        .description(MessageBuilder::default()
                            .push_bold_safe(&card.type_line()) //TODO color indicator
                            .push("\n\n")
                            .push(with_manamoji(&card.text())) //TODO add support for levelers, fix loyalty costs and quotes, italicize ability words and reminder text
                        )
                        .fields(
                            (if let Some((pow, tou)) = card.pt() {
                                Some(CreateEmbedField::default().name("P/T").value(MessageBuilder::default().push_safe(pow).push("/").push_safe(tou)))
                            } else {
                                None
                            }).into_iter()
                            .chain(if let Some(loy) = card.loyalty() {
                                Some(CreateEmbedField::default().name("Loyalty").value(loy))
                            } else {
                                None
                            })
                            .chain(if let Some((hand, life)) = card.vanguard_modifiers() {
                                vec![
                                    CreateEmbedField::default().name("Hand modifier").value(format!("{:+}", hand)),
                                    CreateEmbedField::default().name("Life modifier").value(format!("{:+}", life))
                                ]
                            } else {
                                Vec::default()
                            })
                        )
                        //TODO printings
                    )
                )?;
            } //TODO reply with card stats & resolved Lore Seeker URL
            (None, _) => { msg.reply("no cards found")?; }
        }
    //} else if let (Some(start_idx), Some(end_idx)) = (msg.content.find("[["), msg.content.find("]]")) {
        //TODO card lookup (https://github.com/fenhl/lore-seeker-discord/issues/2)
    }
    Ok(())
}

fn next_word(subj: &str) -> Option<String> {
    let mut word = String::default();
    for c in subj.chars() {
        if c == ' ' { break; }
        word.push(c);
    }
    if word.is_empty() { None } else { Some(word) }
}

fn owner_check(ctx: &Context, msg: &Message) -> Result<(), Error<'static>> {
    let data = ctx.data.lock();
    let owners = data.get::<Owners>().ok_or(Error::MissingOwners)?;
    if owners.contains(&msg.author.id) {
        Ok(())
    } else {
        Err(Error::OwnerCheck)
    }
}

fn main() {
    // read config
    let token = env::var("DISCORD_TOKEN").expect("missing DISCORD_TOKEN envar");
    let mut client = Client::new(&token, Handler);
    let owners = {
        let mut owners = HashSet::default();
        owners.insert(serenity::http::get_current_application_info().expect("couldn't get application info").owner.id);
        owners
    };
    {
        let mut data = client.data.lock();
        data.insert::<Owners>(owners);
        // load cards before going online
        print!("[....] loading cards");
        io::stdout().flush().expect("failed to flush stdout");
        let db = Db::from_sets_dir("/opt/git/github.com/fenhl/lore-seeker/stage/data/sets").expect("failed to load card database");
        assert!(db.card("Dryad Arbor").expect("failed to load dummy card").loyalty().is_none());
        let num_cards = db.into_iter().count();
        data.insert::<CardDb>(db);
        println!("\r[ ok ] {} cards loaded", num_cards);
    }
    // connect to Discord
    client.start_autosharded().expect("client error");
}
