#![warn(trivial_casts)]
#![deny(unused)]
#![forbid(unused_extern_crates, unused_import_braces)]

extern crate kuchiki;
extern crate reqwest;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
extern crate serenity;
extern crate urlencoding;

use std::{env, io, process};
use std::fs::{self, File};
use std::io::prelude::*;
use std::str::FromStr;

use kuchiki::traits::TendrilSink;

use serenity::builder::CreateEmbedField;
use serenity::prelude::*;
use serenity::model::{EmojiId, Message, Permissions, ReactionType, Ready};
use serenity::utils::MessageBuilder;

macro_rules! manamoji {
    ($($sym:expr => $name:expr, $id:expr;)+) => {
        #[allow(unused)] //TODO
        fn manamoji(s: &str) -> Option<ReactionType> {
            match s {
                $($sym => Some(ReactionType::Custom {
                    id: EmojiId($id),
                    name: Some($name.to_owned())
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
                        name: Some($name.to_owned())
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

const SETS_DIR: &'static str = "/opt/git/github.com/fenhl/lore-seeker/stage/data/sets";

#[derive(Deserialize)]
enum Rarity {
    #[serde(rename = "Basic Land")]
    Land,
    Common,
    Uncommon,
    Rare,
    #[serde(rename = "Mythic Rare")]
    Mythic,
    Special
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CardData {
    //TODO layout
    name: String,
    //TODO names
    #[serde(default)]
    mana_cost: String,
    #[serde(rename = "type")]
    type_line: String,
    rarity: Rarity,
    #[serde(default)]
    text: String,
    power: Option<String>,
    toughness: Option<String>,
    loyalty: Option<String>,
    hand: Option<i8>,
    life: Option<i8>
}

#[derive(Debug)]
enum CardFromStrErr {
    CardNotFound,
    Io(io::Error),
    Serde(serde_json::Error)
}

impl From<io::Error> for CardFromStrErr {
    fn from(e: io::Error) -> CardFromStrErr {
        CardFromStrErr::Io(e)
    }
}

impl From<serde_json::Error> for CardFromStrErr {
    fn from(e: serde_json::Error) -> CardFromStrErr {
        CardFromStrErr::Serde(e)
    }
}

impl FromStr for CardData {
    type Err = CardFromStrErr;

    fn from_str(card_name: &str) -> Result<CardData, CardFromStrErr> {
        for entry in fs::read_dir(SETS_DIR)? {
            if let serde_json::Value::Object(set_info) = serde_json::from_reader(File::open(entry?.path())?)? {
                if let Some(&serde_json::Value::Array(ref cards)) = set_info.get("cards") {
                    for card in cards {
                        let card_info = if let &serde_json::Value::Object(ref card_info) = card { card_info } else { continue; };
                        if card_info.get("name").map_or(false, |name| name == card_name) {
                            return Ok(serde_json::from_value(card.clone())?);
                        }
                    }
                }
            }
        }
        Err(CardFromStrErr::CardNotFound)
    }
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

    fn on_message(&self, _: Context, msg: Message) {
        if msg.author.bot { return; } // ignore bots to prevent message loops
        let current_user_id = serenity::CACHE.read().expect("failed to get serenity cache").user.id;
        let query = if msg.content.starts_with(&current_user_id.mention()) { //TODO allow <@!id> mentions
            let mut query = &msg.content[current_user_id.mention().len()..];
            if query.starts_with(':') { query = &query[1..]; }
            if query.starts_with(' ') { query = &query[1..]; }
            Some(query)
        } else if msg.author.create_dm_channel().ok().map_or(false, |dm| dm.id == msg.channel_id) {
            Some(&msg.content[..])
        } else {
            None
        };
        if let Some(query) = query {
            let encoded_query = urlencoding::encode(query);
            let mut response = reqwest::get(&format!("http://localhost:18803/list?q={}", encoded_query)).expect("failed to send Lore Seeker request");
            if !response.status().is_success() {
                panic!("Lore Seeker responded with status code {}", response.status());
            }
            let mut response_content = String::default();
            response.read_to_string(&mut response_content).expect("failed to read Lore Seeker response");
            let document = kuchiki::parse_html().one(response_content);
            let mut matches = document.select("ul#search-result")
                .expect("failed to parse Lore Seeker response")
                .next()
                .expect("failed to find search result in Lore Seeker response")
                .as_node()
                .children()
                .filter_map(|node| node.first_child().and_then(|text_node| text_node.as_text().map(|text| text.borrow().trim().to_owned())));
            match (matches.next(), matches.next()) {
                (Some(_), Some(_)) => { msg.reply(&format!("{} cards found: https://loreseeker.fenhl.net/card?q={}", 2 + matches.count(), encoded_query)).expect("failed to reply"); }
                (Some(card_name), None) => {
                    let card = CardData::from_str(&card_name).expect("card not found in database");
                    let card_url = format!("https://loreseeker.fenhl.net/card?q=!{}", urlencoding::encode(&card_name)); //TODO use exact printing URL
                    let mut reply = msg.reply(&format!("1 card found: {}", card_url)).expect("failed to reply");
                    msg.channel_id.broadcast_typing().expect("failed to broadcast typing");
                    reply.edit(|m| m
                        .embed(|e| e
                            .color(match card.rarity {
                                Rarity::Land | Rarity::Common => (1, 1, 1),// Discord turns actual black into light gray
                                Rarity::Uncommon => (140, 159, 172),
                                Rarity::Rare => (178, 152, 81),
                                Rarity::Mythic => (217, 97, 33),
                                Rarity::Special => (144, 99, 156)
                            })
                            .title(format!("{} {}", card.name, with_manamoji(&card.mana_cost)))
                            .url(&card_url)
                            .description(MessageBuilder::default()
                                .push_bold_safe(&card.type_line) //TODO color indicator
                                .push("\n\n")
                                .push(with_manamoji(&card.text)) //TODO add support for levelers, fix loyalty costs and quotes, italicize ability words and reminder text
                            )
                            .fields(
                                (if let (&Some(ref pow), &Some(ref tou)) = (&card.power, &card.toughness) {
                                    Some(CreateEmbedField::default().name("P/T").value(format!("{}/{}", pow, tou)))
                                } else {
                                    None
                                }).into_iter()
                                .chain(if let Some(ref loy) = card.loyalty {
                                    Some(CreateEmbedField::default().name("Loyalty").value(loy.to_owned()))
                                } else {
                                    None
                                })
                                .chain(if let (Some(hand), Some(life)) = (card.hand, card.life) {
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
                    ).expect("failed to edit reply");
                } //TODO reply with card stats & resolved Lore Seeker URL
                (None, _) => { msg.reply("no cards found").expect("failed to reply"); }
            }
        } else if msg.content.contains("[[") && msg.content.contains("]]") {
            //TODO card lookup
        }
    }
}

fn main() {
    let token = env::var("DISCORD_TOKEN").expect("missing DISCORD_TOKEN envar");
    let mut client = Client::new(&token, Handler);
    client.start_autosharded().expect("client error");
}
