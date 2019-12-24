#![deny(rust_2018_idioms, unused, unused_import_braces, unused_qualifications, warnings)]

mod user_list;

use {
    std::{
        collections::{
            HashMap,
            HashSet
        },
        env,
        fmt,
        fs::File,
        io::{
            self,
            BufReader,
            prelude::*
        },
        iter,
        net::{
            TcpListener,
            TcpStream
        },
        path::PathBuf,
        process::{
            Command,
            Stdio
        },
        str::FromStr,
        sync::Arc,
        thread,
        time::Duration
    },
    itertools::Itertools as _,
    kuchiki::traits::TendrilSink as _,
    log::info,
    mtg::{
        card::{
            Card,
            Db,
            Layout
        },
        color::ColorSet
    },
    rand::prelude::*,
    serde::Deserialize,
    serenity::{
        builder::CreateEmbed,
        cache::CacheRwLock,
        client::bridge::gateway::ShardManager,
        model::prelude::*,
        prelude::*,
        utils::{
            EmbedMessageBuilding as _,
            MessageBuilder
        }
    },
    typemap::{
        Key,
        ShareMap
    },
    url::Url,
    lore_seeker::*
};

macro_rules! manamoji {
    ($($sym:expr => :$name:tt:$id:expr $(, :$alt_name:tt:$alt_id:expr)*;)+) => {
        #[allow(unused)] //TODO
        fn manamoji(&self, s: &str) -> Option<Emoji> {
            match s {
                $($sym => self
                    .as_ref()
                    .read()
                    .guilds
                    .values()
                    .filter_map(|guild| guild.read().emojis.get(&EmojiId($id)).cloned())
                    .next(),
                )+
                _ => None
            }
        }

        fn with_manamoji(&self, s: impl ToString) -> String {
            let s = s.to_string();
            $(
                let mut split = s.split($sym);
                let mut builder = MessageBuilder::default();
                builder.push_safe(split.next().expect("failed to convert manamoji"));
                for part in split {
                    if let Some(emoji) = self.manamoji($sym) {
                        builder.emoji(&emoji);
                    } else {
                        builder.push_safe($sym);
                    }
                    builder.push_safe(part);
                }
                let s = builder.build();
            )+
            s
        }

        fn without_manamoji(&self, s: impl ToString) -> String {
            s.to_string()
                $(
                    .split(concat!("<:", stringify!($name), ':', $id, '>'))
                    .join($sym)
                    $(
                        .split(concat!("<:", stringify!($alt_name), ':', $alt_id, '>'))
                        .join($sym)
                    )*
                )+
        }
    };
}

macro_rules! indicator_emoji {
    ($($colors:pat => $id:expr;)+) => {
        fn indicator_emoji(&self, colors: &ColorSet) -> Option<Emoji> {
            match *colors {
                $($colors => self
                    .as_ref()
                    .read()
                    .guilds
                    .values()
                    .filter_map(|guild| guild.read().emojis.get(&EmojiId($id)).cloned())
                    .next(),
                )+
            }
        }
    };
}

trait EmojiCache {
    fn manamoji(&self, s: &str) -> Option<Emoji>;
    fn with_manamoji(&self, s: impl ToString) -> String;
    fn without_manamoji(&self, s: impl ToString) -> String;
    fn indicator_emoji(&self, colors: &ColorSet) -> Option<Emoji>;
}

impl<T: AsRef<CacheRwLock>> EmojiCache for T {
    manamoji! {
        "{0}" => :mana0:386740600436686878, :0_:233087928111333376;
        "{1}" => :mana1:386740600399069204, :1_:233087941465997312;
        "{10}" => :mana10:386740603708506142, :10:233087978417815552;
        "{11}" => :mana11:386741116927475718, :11:233095414390194176;
        "{12}" => :mana12:386741117577592833, :12:233095464222851074;
        "{13}" => :mana13:386741118303469570, :13:233095464428240896;
        "{14}" => :mana14:386741118668242964, :14:585567535794225167;
        "{15}" => :mana15:386741118273847307, :15:233095536889036803;
        "{16}" => :mana16:386741118597070850;
        "{17}" => :mana17:386741120434044948;
        "{18}" => :mana18:386741120006094860;
        "{19}" => :mana19:386741120371261449;
        "{2}" => :mana2:386740600755453953, :2_:233087976723185665;
        "{2/B}" => :mana2b:386741115807596547, :2B:624281333740339210;
        "{2/G}" => :mana2g:386741116118106122, :2G:624281333769961514;
        "{2/R}" => :mana2r:386741116222963712, :2R:624281334948298752;
        "{2/U}" => :mana2u:386741115933687809, :2U:624281333350268929;
        "{2/W}" => :mana2w:386741116462039043, :2W:624281333824356379;
        "{20}" => :mana20:386741120543227906;
        "{3}" => :mana3:386740601082871822, :3_:233087977377628160;
        "{4}" => :mana4:386740601556828160, :4_:233087977855778826;
        "{5}" => :mana5:386740601724338176, :5_:233087978300243968;
        "{6}" => :mana6:386740602265403392, :6_:233087978342187008;
        "{7}" => :mana7:386740602374717440, :7_:233087978350706688;
        "{8}" => :mana8:386740602747879435, :8_:233087978409426944;
        "{9}" => :mana9:386740603494334484, :9_:233087978413621251;
        "{B}" => :manab:386740604341583873, :B_:233094539970084864, :B_:619230145496088591;
        "{B/G}" => :manabg:386740605847470081, :BG:233095174572605440;
        "{B/P}" => :manabp:386741121486815232, :PB:233095745371111424;
        "{B/R}" => :manabr:386740607017549838, :BR:233082275330392064;
        "{C}" => :manac:386740607416139777, :C_:233094585830735872;
        "{CHAOS}" => :manachaos:386741121608318986;
        "{DISCOVER}" => :manadiscover:583092272028057601;
        "{E}" => :manae:386741121780416513, :E_:233094452661583872;
        "{G}" => :manag:386740607827181569, :G_:233094570705944576, :G_:619230145906999316;
        "{G/P}" => :managp:386741122225012761, :PG:233095788140560385;
        "{G/U}" => :managu:386740607906873344, :GU:233095244332138497;
        "{G/W}" => :managw:386740607994953728, :GW:233082340417601537;
        "{P}" => :manap:483816536176590898;
        "{Q}" => :manaq:386741125987434506, :Q_:341473311869501440;
        "{R}" => :manar:386740612394778634, :R_:233094555346403328, :R_:619230146448064512;
        "{R/G}" => :manarg:386740613430640640, :RG:233082296717017099;
        "{R/P}" => :manarp:386741125773262860, :PR:233095765919006720;
        "{R/W}" => :manarw:386740615498563588, :RW:233095204649828352;
        "{S}" => :manas:386741126939541505, :S_:592564384824295426;
        "{T}" => :manat:386740612143120385, :T_:233088054674456577;
        "{U}" => :manau:386740612289789953, :U_:233094523012644864, :U_:619230146410577930;
        "{U/B}" => :manaub:386740615683112992, :UB:233082236444868618;
        "{U/P}" => :manaup:386741126247350284, :PU:233095720033452033;
        "{U/R}" => :manaur:386740615649558534, :UR:233095125914484737;
        "{V}" => :manarunic:483811996924379156;
        "{W}" => :manaw:386740617792978944, :W_:233094502640910336, :W_:619230146188279848;
        "{W/B}" => :manawb:386740617780264960, :WB:233095081316319244;
        "{W/P}" => :manawp:386741126645809162, :PW:233095689058517002;
        "{W/U}" => :manawu:386740617792978964, :WU:233082066831409162;
        "{X}" => :manax:386740617667018752, :X_:233088003591897098;
        "{Y}" => :manay:386741126457065473;
        "{Z}" => :manaz:386741127207845890;
        "{hr}" => :manahr:386741125672730634;
        "{hw}" => :manahw:386741125773262848;
        "{½}" => :manahalf:386741122510225421;
        "{∞}" => :manainfinity:386741125861605387;
    }

    indicator_emoji! {
        ColorSet { white: false, blue: false, black: false, red: false, green: false } => 493537120628244481;
        ColorSet { white: true, blue: false, black: false, red: false, green: false } => 493480100252352512;
        ColorSet { white: false, blue: true, black: false, red: false, green: false } => 493480399532589067;
        ColorSet { white: false, blue: false, black: true, red: false, green: false } => 493480399146844171;
        ColorSet { white: false, blue: false, black: false, red: true, green: false } => 493480399545040910;
        ColorSet { white: false, blue: false, black: false, red: false, green: true } => 493480399737978883;
        ColorSet { white: true, blue: true, black: false, red: false, green: false } => 493518454775873536;
        ColorSet { white: false, blue: true, black: true, red: false, green: false } => 493518454033219585;
        ColorSet { white: false, blue: false, black: true, red: true, green: false } => 493518453249015809;
        ColorSet { white: false, blue: false, black: false, red: true, green: true } => 493518453886550016;
        ColorSet { white: true, blue: false, black: false, red: false, green: true } => 493518453919973407;
        ColorSet { white: true, blue: false, black: true, red: false, green: false } => 493518454108979202;
        ColorSet { white: false, blue: true, black: false, red: true, green: false } => 493518454268100650;
        ColorSet { white: false, blue: false, black: true, red: false, green: true } => 493518453123186689;
        ColorSet { white: true, blue: false, black: false, red: true, green: false } => 493518453999927307;
        ColorSet { white: false, blue: true, black: false, red: false, green: true } => 493518453249015810;
        ColorSet { white: true, blue: true, black: false, red: false, green: true } => 493531781744689153;
        ColorSet { white: true, blue: true, black: true, red: false, green: false } => 493531782210125864;
        ColorSet { white: false, blue: true, black: true, red: true, green: false } => 493531782508052520;
        ColorSet { white: false, blue: false, black: true, red: true, green: true } => 493531781799084032;
        ColorSet { white: true, blue: false, black: false, red: true, green: true } => 493531781660672005;
        ColorSet { white: true, blue: false, black: true, red: false, green: true } => 493531782386548736;
        ColorSet { white: true, blue: true, black: false, red: true, green: false } => 493531782130696193;
        ColorSet { white: false, blue: true, black: true, red: false, green: true } => 493531780666884097;
        ColorSet { white: true, blue: false, black: true, red: true, green: false } => 493531782352732160;
        ColorSet { white: false, blue: true, black: false, red: true, green: true } => 493531781782306863;
        ColorSet { white: true, blue: true, black: true, red: true, green: false } => 493536592296673290;
        ColorSet { white: false, blue: true, black: true, red: true, green: true } => 493536591973974046;
        ColorSet { white: true, blue: false, black: true, red: true, green: true } => 493536591403286563;
        ColorSet { white: true, blue: true, black: false, red: true, green: true } => 493536591482978315;
        ColorSet { white: true, blue: true, black: true, red: false, green: true } => 493536591474589699;
        ColorSet { white: true, blue: true, black: true, red: true, green: true } => 493540446623236096;
    }
}

const EXH_CARDS_CHANNEL: ChannelId = ChannelId(639419990843326464);

trait MessageBuilderExt {
    fn push_card_link(&mut self, card: &Card) -> &mut Self;
}

impl MessageBuilderExt for MessageBuilder {
    fn push_card_link(&mut self, card: &Card) -> &mut Self {
        self.push_named_link_safe(card, card_name_url(card).expect("failed to generate card URL"))
    }
}

struct CardDb;

impl Key for CardDb {
    type Value = Db;
}

struct InlineChannels;

impl Key for InlineChannels {
    type Value = HashSet<ChannelId>;
}

struct InlineGuilds;

impl Key for InlineGuilds {
    type Value = HashSet<GuildId>;
}

struct Owners;

impl Key for Owners {
    type Value = HashSet<UserId>;
}

struct ShardManagerContainer;

impl Key for ShardManagerContainer {
    type Value = Arc<Mutex<ShardManager>>;
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Config {
    #[serde(default)]
    inline_channels: HashSet<ChannelId>,
    #[serde(default)]
    inline_guilds: HashSet<GuildId>,
    bot_token: String
}

impl Config {
    fn new() -> Result<Config, Error> {
        let path = config_path();
        Ok(serde_json::from_reader(File::open(&path).at(path)?)?)
    }
}

#[derive(Default)]
struct Handler(Arc<Mutex<Option<Context>>>);

impl EventHandler for Handler {
    fn ready(&self, ctx: Context, ready: Ready) {
        *self.0.lock() = Some(ctx.clone());
        let guilds = ready.user.guilds(&ctx).expect("failed to get guilds");
        if guilds.is_empty() {
            println!("No guilds found, use following URL to invite the bot:");
            println!("{}", ready.user.invite_url(&ctx, Permissions::READ_MESSAGES | Permissions::SEND_MESSAGES | Permissions::EMBED_LINKS | Permissions::ATTACH_FILES | Permissions::USE_EXTERNAL_EMOJIS | Permissions::ADD_REACTIONS).expect("failed to generate invite URL"));
            shut_down(&ctx);
        }
    }

    fn guild_ban_addition(&self, _: Context, guild_id: GuildId, user: User) {
        user_list::remove(guild_id, user).expect("failed to remove banned user from user list");
    }

    fn guild_ban_removal(&self, ctx: Context, guild_id: GuildId, user: User) {
        user_list::add(guild_id, guild_id.member(ctx, user).expect("failed to get unbanned guild member")).expect("failed to add unbanned user to user list");
    }

    fn guild_create(&self, _: Context, guild: Guild, _: bool) {
        user_list::set_guild(guild.id, guild.members.values().cloned()).expect("failed to initialize user list");
        println!("Connected to {}", guild.name);
    }

    fn guild_member_addition(&self, _: Context, guild_id: GuildId, member: Member) {
        user_list::add(guild_id, member).expect("failed to add new guild member to user list");
    }

    fn guild_member_removal(&self, _: Context, guild_id: GuildId, user: User, _: Option<Member>) {
        user_list::remove(guild_id, user).expect("failed to remove removed guild member from user list");
    }

    fn guild_member_update(&self, _: Context, _: Option<Member>, member: Member) {
        user_list::update(member.guild_id, member).expect("failed to update guild member info in user list");
    }

    fn guild_members_chunk(&self, _: Context, guild_id: GuildId, members: HashMap<UserId, Member>) {
        for member in members.values() {
            user_list::add(guild_id, member.clone()).expect("failed to add chunk of guild members to user list");
        }
    }

    fn message(&self, ctx: Context, msg: Message) {
        if msg.author.bot { return; } // ignore bots to prevent message loops
        if let Some(err_reply) = match handle_message(&ctx, &msg) {
            Ok(()) => None,
            Err(Error::DmOnlyCommand) => Some("to avoid spamming channels, this command is only allowed in direct messages".into()),
            Err(Error::MomirMissingCmc) => Some("missing CMC".into()),
            Err(Error::NoSuchCard(card_name)) => Some(MessageBuilder::default().push("error: no such card: ").push_safe(card_name).build()),
            Err(Error::OwnerCheck) => Some("this command can only be used by the bot owner".into()),
            Err(Error::ParseInt(e)) => Some(MessageBuilder::default().push("invalid number: ").push_safe(e).build()),
            Err(Error::Reqwest(e)) => {
                if e.is_timeout() {
                    println!("Message handler timed out: {:?}", e);
                    Some("Lore Seeker website not responding, try again later".into())
                } else {
                    println!("Message handler returned reqwest error {:?}", e);
                    Some("failed to connect to Lore Seeker website, try again later".into()) //TODO check if lore-seeker service is running
                }
            }
            Err(Error::UnknownCommand(cmd)) => Some(
                MessageBuilder::default()
                    .push("Unknown command: %")
                    .push_safe(cmd)
                    .push(". See <https://github.com/fenhl/lore-seeker-discord/wiki> for my documentation, including a list of commands.")
                    .build()
            ),
            Err(e) => {
                println!("Message handler returned error {:?}", e);
                Some("unknown error (cc <@86841168427495424>)".into())
            }
        } {
            msg.reply(ctx, &err_reply).expect("failed to send error reply");
        }
    }
}

#[must_use]
fn base_path() -> PathBuf {
    env::var_os("LORESEEKERDATA")
        .unwrap_or_else(|| "/usr/local/share/fenhl/lore-seeker".into())
        .into()
}

#[must_use]
fn card_embed<'a>(ctx: &impl AsRef<CacheRwLock>, e: &'a mut CreateEmbed, card: Card, card_url: &Url) -> &'a mut CreateEmbed {
    e
        .color(match card.rarity().color() {
            (0, 0, 0) => (1, 1, 1), // Discord turns actual black into light gray
            c => c
        })
        .title(if let Some(cost) = card.mana_cost() {
            format!("{} {}", card, ctx.with_manamoji(cost))
        } else {
            card.to_string()
        })
        .url(card_url)
        .description({
            let mut description_builder = MessageBuilder::default();
            match card.layout() {
                Layout::Normal => {}
                Layout::Split { left, right } => {
                    description_builder.push(if card.is_alt() { "(Right half of " } else { "(Left half of " })
                        .push_card_link(&left)
                        .push(" // ")
                        .push_card_link(&right)
                        .push_line(')');
                }
                Layout::Flip { unflipped, flipped } => if card.is_alt() {
                    description_builder.push("(Flipped version of ")
                        .push_card_link(&unflipped)
                        .push_line(')');
                } else {
                    description_builder.push("(Flips into ")
                        .push_card_link(&flipped)
                        .push_line(')');
                },
                Layout::DoubleFaced { front, back, .. } => if card.is_alt() {
                    description_builder.push("(Transforms from ")
                        .push_card_link(&front)
                        .push_line(')');
                } else {
                    description_builder.push("(Transforms into ")
                        .push_card_link(&back)
                        .push_line(')');
                },
                Layout::Meld { top, bottom, back } => if card.is_alt() {
                    description_builder.push("(Melds from ")
                        .push_card_link(&top)
                        .push(" and ")
                        .push_card_link(&bottom)
                        .push_line(')');
                } else {
                    description_builder.push("(Melds with ")
                        .push_card_link(if card == top { &bottom } else { &top })
                        .push(" into ")
                        .push_card_link(&back)
                        .push_line(')');
                },
                Layout::Adventure { creature, adventure } => if card.is_alt() {
                    description_builder.push("(Adventure of ")
                        .push_card_link(&creature)
                        .push_line(')');
                } else {
                    description_builder.push("(Has Adventure: ")
                        .push_card_link(&adventure)
                        .push_line(')');
                }
            }
            if let Some(indicator) = card.color_indicator() {
                if let Some(emoji) = ctx.indicator_emoji(&indicator) {
                    description_builder.emoji(&emoji);
                } else {
                    description_builder.push(format!("({}) ", indicator.canonical_order().into_iter().map(|c| c.letter()).collect::<String>()));
                }
            }
            description_builder
                .push_bold_safe(&card.type_line())
                .push("\n\n")
                .push(ctx.with_manamoji(&card.text())) //TODO add support for levelers, fix loyalty costs and quotes, italicize ability words and reminder text
                .build()
        })
        .fields(
            (if let Some((pow, tou)) = card.pt() {
                Some(("P/T", MessageBuilder::default().push_safe(pow).push("/").push_safe(tou).build(), true))
            } else {
                None
            }).into_iter()
            .chain(card.loyalty().map(|loy| ("Loyalty", loy.to_string(), true)))
            .chain(card.stability().map(|sta| ("Stability", sta.to_string(), true)))
            .chain(card.vanguard_modifiers().map(|(hand, life)| {
                vec![
                    (("Hand modifier", format!("{:+}", hand), true)),
                    (("Life modifier", format!("{:+}", life), true))
                ]
            }).unwrap_or_default())
        )
        .footer(|f| f.text(
            if card.num_printings() >= 100 {
                format!("{} printings", card.num_printings())
            } else {
                card.printings()
                    .into_iter()
                    .map(|printing| format!("{}-{}", printing.set, printing.rarity.short()))
                    .collect::<Vec<_>>()
                    .join(", ")
            }
        ))
}

fn card_name_url(card: impl ToString) -> Result<Url, url::ParseError> {
    format!("https://{}/card?q=!{}", hostname(), urlencoding::encode(&card.to_string())).parse()
}

#[must_use]
fn config_path() -> PathBuf {
    base_path().join("config.json")
}

fn eat_user_mention(subj: &mut &str) -> Option<UserId> {
    if !subj.starts_with('<') || !subj.contains('>') {
        return None;
    }
    let mut maybe_mention = String::default();
    let mut chars = subj.chars();
    while let Some(c) = chars.next() {
        maybe_mention.push(c);
        if c == '>' {
            if let Ok(id) = UserId::from_str(&maybe_mention) {
                *subj = &subj[maybe_mention.len()..]; // consume mention text
                return Some(id);
            }
            return None;
        }
    }
    None
}

fn eat_whitespace(subj: &mut &str) {
    while subj.starts_with(' ') {
        *subj = &subj[1..];
    }
}

fn eat_word(subj: &mut &str) -> Option<String> {
    if let Some(word) = next_word(*subj) {
        *subj = &subj[word.len()..];
        eat_whitespace(subj);
        Some(word)
    } else {
        None
    }
}

fn handle_ipc_client(ctx_arc: &Mutex<Option<Context>>, stream: TcpStream) -> Result<(), Error> {
    for line in BufReader::new(&stream).lines() {
        let line = match line {
            Ok(line) => line,
            Err(e) => if e.kind() == io::ErrorKind::ConnectionReset {
                break; // connection reset by peer, consider the IPC session terminated
            } else {
                return Err(e.annotate("IPC client line"));
            }
        };
        let args = shlex::split(&line).ok_or(Error::Shlex)?;
        match &args[0][..] {
            "announce-exh-cards" => {
                let ctx_guard = ctx_arc.lock();
                let ctx = ctx_guard.as_ref().ok_or(Error::MissingContext)?;
                for card_name in &args[1..] {
                    show_single_card(&ctx, EXH_CARDS_CHANNEL, None, card_name, &card_name_url(card_name)?)?;
                }
                writeln!(&mut &stream, "card(s) announced").annotate("announce-exh-cards IPC reply")?;
            }
            "quit" => {
                let ctx_guard = ctx_arc.lock();
                let ctx = ctx_guard.as_ref().ok_or(Error::MissingContext)?;
                shut_down(&ctx);
                thread::sleep(Duration::from_secs(1)); // wait to make sure websockets can be closed cleanly
                writeln!(&mut &stream, "shutdown complete").annotate("quit IPC reply")?;
            }
            "reload" => {
                let ctx_guard = ctx_arc.lock();
                let ctx = ctx_guard.as_ref().ok_or(Error::MissingContext)?;
                reload_all(ctx)?;
                writeln!(&mut &stream, "reload complete").annotate("reload IPC reply")?;
            }
            s => { return Err(Error::UnknownCommand(s.to_owned())); }
        }
    }
    Ok(())
}

fn handle_message(ctx: &Context, msg: &Message) -> Result<(), Error> {
    info!("handling message {:?}", msg.content);
    let current_user_id = AsRef::<CacheRwLock>::as_ref(ctx).read().user.id;
    let is_inline_channel = {
        // guild in inlineGuilds xor channel in inlineChannels
        let ctx_data = ctx.data.read();
        let inline_guilds = ctx_data.get::<InlineGuilds>().ok_or(Error::MissingInlineChannels)?;
        msg.guild_id.map_or(false, |guild_id| inline_guilds.contains(&guild_id))
        != ctx_data.get::<InlineChannels>().ok_or(Error::MissingInlineChannels)?.contains(&msg.channel_id)
    };
    let mut query = &msg.content[..];
    if eat_user_mention(&mut query) == Some(current_user_id) {
        if query.starts_with(':') { query = &query[1..]; }
        if query.starts_with(' ') { query = &query[1..]; }
    } else if is_inline_channel && msg.content.starts_with('%') || msg.is_private() {
        // use full message as query
    } else {
        query = ""
    };
    if query.starts_with('%') {
        // command
        query = &query[1..];
        if let Some(cmd_name) = eat_word(&mut query) {
            match &cmd_name[..] {
                "quit" => {
                    owner_check(ctx, &msg)?;
                    shut_down(ctx);
                    return Ok(());
                }
                "ping" => {
                    msg.reply(ctx, "pong")?;
                    return Ok(());
                }
                "help" | "commands" => {
                    msg.reply(ctx, "See <https://github.com/fenhl/lore-seeker-discord/wiki> for my documentation, including a list of commands.")?;
                }
                "check" => {
                    owner_check(ctx, &msg)?;
                    if !msg.is_private() {
                        return Err(Error::DmOnlyCommand);
                    }
                    msg.reply(ctx, "reloading database…")?;
                    {
                        let mut data = ctx.data.write();
                        reload_db(&mut data)?;
                    }
                    msg.reply(ctx, "done, resolving query…")?;
                    let (encoded_query, matches) = resolve_query(query)?;
                    msg.reply(ctx, &format!("{} cards found: <https://{}/card?q={}>. Checking cards…", matches.len(), hostname(), encoded_query))?;
                    let data = ctx.data.read();
                    let db = data.get::<CardDb>().ok_or(Error::MissingCardDb)?;
                    let mut oks = 0;
                    let mut errs = 0;
                    for (card_name, card_url) in matches {
                        let card = match db.card(&card_name) {
                            Some(card) => card,
                            None => {
                                msg.reply(ctx, &format!("Card not found: {}", card_name))?;
                                errs += 1;
                                continue;
                            }
                        };
                        let ctx_clone = ctx.clone();
                        let check_thread = thread::Builder::new().name("Lore Seeker card check".into()).spawn(move || {
                            let _ = card_embed(&ctx_clone, &mut CreateEmbed::default(), card, &card_url);
                        }).annotate("spawn card check thread")?;
                        match check_thread.join() {
                            Ok(()) => { oks += 1; }
                            Err(_) => {
                                msg.reply(ctx, &format!("Error rendering card embed for {}", card_name))?;
                                errs += 1;
                            }
                        }
                    }
                    msg.reply(ctx, &format!("{} card embeds successfully rendered, {} errors", oks, errs))?;
                    return Ok(());
                }
                "rand" | "random" => {
                    return handle_query(ctx, msg, &ctx.without_manamoji(query), true);
                }
                "reload" | "update" => {
                    owner_check(ctx, &msg)?;
                    reload_all(ctx)?;
                    msg.react(ctx, "✅")?;
                    return Ok(());
                }
                "booster" | "boosters" | "pack" | "packs" | "sealed" => {
                    while let Some(set_code) = eat_word(&mut query) {
                        let lower_code = set_code.to_ascii_lowercase();
                        let document = {
                            let mut response = get(format!("/sealed?count[]=1&set[]={}", lower_code))?;
                            let mut response_content = String::default();
                            response.read_to_string(&mut response_content).annotate("sealed response")?;
                            kuchiki::parse_html().one(response_content)
                        };
                        let base_url = Url::parse(&format!("https://{}/", hostname()))?;
                        let set_name = document.select_first(&format!(".pack_selection option[value=\"{}\"]", lower_code)).ok()
                            .map_or(set_code, |node_data| node_data.text_contents());
                        let cards = document.select(".card_picture_cell").map_err(|()| Error::MissingCardList)?.map(|cell| {
                            let cell_node: &kuchiki::NodeRef = cell.as_node();
                            let a_node_data = cell_node.select_first("a").map_err(|()| Error::MissingCardLink)?;
                            let a_node = a_node_data.as_node();
                            let a_elt = a_node.as_element().ok_or(Error::MissingANode)?;
                            let href = a_elt.attributes.borrow().get("href").ok_or(Error::MissingHref).and_then(|href| base_url.join(href).map_err(Error::from))?;
                            let img_node_data = a_node.select_first("img").map_err(|()| Error::MissingTextNode)?;
                            let img_node = img_node_data.as_node();
                            let img_elt = img_node.as_element().ok_or(Error::MissingTextNode)?;
                            let card_name = img_elt.attributes.borrow().get("alt").ok_or(Error::MissingTextNode)?.to_string();
                            Ok::<_, Error>((card_name, href))
                        }).collect::<Result<Vec<_>, _>>()?;
                        msg.channel_id.send_message(ctx, |m| {
                            m.embed(|e| e
                                .title(MessageBuilder::default().push_italic_safe(set_name).push(" booster"))
                                .description({
                                    let mut builder = MessageBuilder::default();
                                    for (card_name, href) in cards {
                                        builder.push('[').push_safe(card_name).push_line(format!("]({})", href));
                                    }
                                    builder
                                })
                            )
                        })?;
                    }
                    return Ok(());
                }
                "mjt" => {
                    // Mental Judge Tower, for real cards
                    let mut players = Vec::default();
                    while let Some(user_id) = eat_user_mention(&mut query) {
                        players.push(user_id);
                        eat_whitespace(&mut query);
                    }
                    return mental_judge_tower(ctx, &msg, players, Some(false), &ctx.without_manamoji(query));
                }
                "cmjt" => {
                    // custom Mental Judge Tower, for custom cards
                    let mut players = Vec::default();
                    while let Some(user_id) = eat_user_mention(&mut query) {
                        players.push(user_id);
                        eat_whitespace(&mut query);
                    }
                    return mental_judge_tower(ctx, &msg, players, Some(true), &ctx.without_manamoji(query));
                }
                "fmjt" => {
                    // fusion Mental Judge Tower, for both real and custom cards
                    let mut players = Vec::default();
                    while let Some(user_id) = eat_user_mention(&mut query) {
                        players.push(user_id);
                        eat_whitespace(&mut query);
                    }
                    return mental_judge_tower(ctx, &msg, players, None, &ctx.without_manamoji(query));
                }
                "momir" => {
                    // Momir, for real cards
                    let cmc = eat_word(&mut query).ok_or(Error::MomirMissingCmc)?.parse::<usize>()?;
                    eat_whitespace(&mut query);
                    return momir(ctx, msg, cmc, Some(false), &ctx.without_manamoji(query));
                }
                "cmomir" => {
                    // custom Momir, for custom cards
                    let cmc = eat_word(&mut query).ok_or(Error::MomirMissingCmc)?.parse::<usize>()?;
                    eat_whitespace(&mut query);
                    return momir(ctx, msg, cmc, Some(true), &ctx.without_manamoji(query));
                }
                "fmomir" => {
                    // fusion Momir, for both real and custom cards
                    let cmc = eat_word(&mut query).ok_or(Error::MomirMissingCmc)?.parse::<usize>()?;
                    eat_whitespace(&mut query);
                    return momir(ctx, msg, cmc, None, &ctx.without_manamoji(query));
                }
                cmd => {
                    return Err(Error::UnknownCommand(cmd.into()));
                }
            }
        }
    } else if query.len() > 0 {
        handle_query(ctx, msg, &ctx.without_manamoji(query), false)?;
    } else if is_inline_channel {
        let mut remaining_msg = &msg.content[..];
        while let Some(start_idx) = remaining_msg.find("[[") {
            if let Some(end_offset) = remaining_msg[start_idx..].find("]]") {
                let end_idx = start_idx + end_offset;
                let query = &remaining_msg[start_idx + 2..end_idx];
                if let Some(card) = {
                    let ctx_data = ctx.data.read();
                    let db = ctx_data.get::<CardDb>().ok_or(Error::MissingCardDb)?;
                    db.card_fuzzy(query)
                } {
                    handle_query_result(ctx, msg,
                        iter::once((card.to_string(), card_name_url(&card).expect("failed to generate card URL"))), //TODO use exact printing URL
                        false,
                        format!("!{}", urlencoding::encode(query)) //TODO fix query
                    )?;
                } else {
                    handle_query(ctx, msg, &ctx.without_manamoji(query), false)?;
                }
                remaining_msg = &remaining_msg[end_idx + 2..];
            } else {
                break;
            }
        }
    }
    Ok(())
}

fn handle_query(ctx: &Context, msg: &Message, query: &str, random: bool) -> Result<(), Error> {
    let (encoded_query, matches) = resolve_query(query)?;
    handle_query_result(ctx, msg, matches, random, encoded_query)
}

fn handle_query_result(ctx: &Context, msg: &Message, matches: impl IntoIterator<Item = (String, Url)>, random: bool, encoded_query: impl fmt::Display) -> Result<(), Error> {
    let mut matches = matches.into_iter();
    if random {
        let matches_vec = matches.collect::<Vec<_>>();
        if let Some(&(ref card_name, ref card_url)) = matches_vec.choose(&mut thread_rng()) {
            show_single_card(ctx, msg.channel_id, Some((&msg, Some(&format!("{} card{} found, random card", matches_vec.len(), if matches_vec.len() == 1 { "" } else { "s" })))), card_name, card_url)?;
        } else {
            msg.reply(ctx, "no cards found")?;
        }
    } else {
        match (matches.next(), matches.next()) {
            (Some(_), Some(_)) => { msg.reply(ctx, &format!("{} cards found: <https://{}/card?q={}>", 2 + matches.count(), hostname(), encoded_query))?; }
            (Some((card_name, card_url)), None) => {
                show_single_card(ctx, msg.channel_id, Some((&msg, None)), &card_name, &card_url)?;
            }
            (None, _) => { msg.reply(ctx, "no cards found")?; }
        }
    }
    Ok(())
}

fn ipc_addr() -> &'static str {
    if is_dev() {
        "127.0.0.1:18810"
    } else {
        "127.0.0.1:18806"
    }
}

fn listen_ipc(ctx_arc: Arc<Mutex<Option<Context>>>) -> Result<(), Error> { //TODO change return type to Result<!, Error>
    for stream in TcpListener::bind(ipc_addr()).annotate("IPC listener")?.incoming() {
        if let Err(e) = stream.map_err(|e| e.annotate("incoming stream")).and_then(|stream| handle_ipc_client(&ctx_arc, stream)) {
            notify_ipc_crash(e);
        }
    }
    unreachable!();
}

fn mental_judge_tower(ctx: &Context, msg: &Message, mut players: Vec<UserId>, custom: Option<bool>, query: &str) -> Result<(), Error> {
    let mut builder = MessageBuilder::default();
    let mut gen = thread_rng();
    if !players.is_empty() {
        players.shuffle(&mut gen);
        for (i, player) in players.into_iter().enumerate() {
            builder
                .push(if i == 0 { "turn order: " } else { ", " })
                .mention(&player);
        }
        builder.push("\n");
    }
    builder.push(format!(
        "seed: <https://{}/card?q={}+is%3Amainfront+not%3Areprint+sort%3Arand",
        hostname(),
        match custom {
            Some(true) => "st%3Acustom",
            Some(false) => "%28f%3AVintage+or+%28banned%3AVintage+-o%3A%2F%5CWante%5CW%2F%29%29",
            None => "%28f%3AVintage+or+%28banned%3AVintage+-o%3A%2F%5CWante%5CW%2F%29+or+st%3Acustom%29"
        }
    ));
    if !query.is_empty() {
        builder.push("+").push(urlencoding::encode(query));
    }
    builder.push(format!(
        "&random_seed={:08x}{:08x}>",
        gen.gen_range(0, 0x1_0000_0000_u64),
        gen.gen_range(0, 0x1_0000_0000_u64)
    ));
    msg.reply(ctx, &builder.build())?;
    Ok(())
}

fn momir(ctx: &Context, msg: &Message, cmc: usize, custom: Option<bool>, query: &str) -> Result<(), Error> {
    let query = format!(
        "is:mainfront t:creature cmc={} {}{}{}",
        cmc,
        match custom {
            Some(true) => "is:custom",
            Some(false) => "f:Vintage",
            None => "(is:custom or f:Vintage)"
        },
        if query.is_empty() { "" } else { " " },
        query
    );
    handle_query(ctx, msg, &query, true)
}

fn next_word(subj: &str) -> Option<String> {
    let mut word = String::default();
    for c in subj.chars() {
        if c == ' ' { break; }
        word.push(c);
    }
    if word.is_empty() { None } else { Some(word) }
}

fn notify_ipc_crash(e: Error) {
    let mut child = Command::new("ssmtp")
        .arg("fenhl@fenhl.net")
        .stdin(Stdio::piped())
        .spawn()
        .expect("failed to spawn ssmtp");
    {
        let stdin = child.stdin.as_mut().expect("failed to open ssmtp stdin");
        write!(
            stdin,
            "To: fenhl@fenhl.net\nFrom: {username}@{hostname}\nSubject: {bot} IPC thread crashed\n\n{bot} IPC thread crashed with the following error:\n{error:?}\n",
            bot=if is_dev() { "Lore Seeker (dev)" } else { "Lore Seeker" },
            error=e,
            hostname=whoami::hostname(),
            username=whoami::username()
        ).expect("failed to write to ssmtp stdin");
    }
    child.wait().expect("failed to wait for ssmtp subprocess"); //TODO check exit status
}

fn owner_check(ctx: &Context, msg: &Message) -> Result<(), Error> {
    let data = ctx.data.read();
    let owners = data.get::<Owners>().ok_or(Error::MissingOwners)?;
    if owners.contains(&msg.author.id) {
        Ok(())
    } else {
        Err(Error::OwnerCheck)
    }
}

fn reload_all(ctx: &Context) -> Result<(), Error> {
    let config = Config::new()?;
    let mut data = ctx.data.write();
    reload_config(&mut data, config)?;
    reload_db(&mut data)?;
    Ok(())
}

fn reload_config(ctx_data: &mut ShareMap, config: Config) -> Result<(), Error> {
    ctx_data.insert::<InlineChannels>(config.inline_channels);
    ctx_data.insert::<InlineGuilds>(config.inline_guilds);
    Ok(())
}

fn reload_db(ctx_data: &mut ShareMap) -> Result<(), Error> {
    let db = Db::from_sets_dir(sets_path(), true)?;
    ctx_data.insert::<CardDb>(db);
    Ok(())
}

fn send_ipc_command_no_wait<T: fmt::Display, I: IntoIterator<Item = T>>(cmd: I) -> Result<(), Error> {
    let mut stream = TcpStream::connect(ipc_addr()).annotate("send_ipc_command_no_wait connect")?;
    writeln!(&mut stream, "{}", cmd.into_iter().map(|arg| shlex::quote(&arg.to_string()).into_owned()).collect::<Vec<_>>().join(" ")).annotate("send_ipc_command_no_wait write")?;
    Ok(())
}

fn send_ipc_command_wait<T: fmt::Display, I: IntoIterator<Item = T>>(cmd: I) -> Result<String, Error> {
    let mut stream = TcpStream::connect(ipc_addr()).annotate("send_ipc_command_wait connect")?;
    writeln!(&mut stream, "{}", cmd.into_iter().map(|arg| shlex::quote(&arg.to_string()).into_owned()).collect::<Vec<_>>().join(" ")).annotate("send_ipc_command_wait write")?;
    let mut buf = String::default();
    BufReader::new(stream).read_line(&mut buf).annotate("send_ipc_command_wait read")?;
    if buf.pop() != Some('\n') { return Err(Error::MissingNewline) }
    Ok(buf)
}

fn sets_path() -> PathBuf {
    base_path().join("repo").join("data").join("sets")
}

fn show_single_card(ctx: &Context, channel_id: ChannelId, reply: Option<(&Message, Option<&str>)>, card_name: &str, card_url: &Url) -> Result<(), Error> {
    let card = {
        let data = ctx.data.read();
        data.get::<CardDb>().and_then(|db| db.card(card_name))
    };
    channel_id.send_message(ctx, |m| {
        let m = match (reply, card.is_some()) {
            (None, true) | (Some((_, None)), _) => m, // card can be rendered and no explicit reply text exists, so just send the render
            (None, false) => m.content(format!("1 card found: <{}>", card_url)),
            (Some((msg, Some(reply_text))), _) => m.content(MessageBuilder::default().mention(&msg.author).push(format!(": {}: <{}>", reply_text, card_url)))
        };
        if let Some(card) = card {
            m.embed(|e| card_embed(ctx, e, card, card_url))
        } else {
            m
        }
    })?;
    Ok(())
}

/// Utility function to shut down all shards.
pub fn shut_down(ctx: &Context) {
    ctx.invisible(); // hack to prevent the bot showing as online when it's not
    let data = ctx.data.read();
    let mut shard_manager = data.get::<ShardManagerContainer>().expect("missing shard manager").lock();
    shard_manager.shutdown_all();
}

fn main() -> Result<(), Error> {
    env_logger::init();
    let mut args = env::args().peekable();
    let _ = args.next(); // ignore executable name
    if let Some(subcmd) = args.peek() {
        if subcmd == "--no-wait" {
            let _ = args.next(); // eat `--no-wait` arg
            send_ipc_command_no_wait(args)?;
        } else {
            println!("{}", send_ipc_command_wait(args)?);
        }
    } else {
        // read config
        let config = Config::new()?;
        let handler = Handler::default();
        let ctx_arc = handler.0.clone();
        let mut client = Client::new(&config.bot_token, handler)?;
        let owners = iter::once(client.cache_and_http.http.get_current_application_info()?.owner.id).collect();
        {
            let mut data = client.data.write();
            data.insert::<Owners>(owners);
            data.insert::<ShardManagerContainer>(Arc::clone(&client.shard_manager));
            reload_config(&mut data, config)?;
            // load cards before going online
            println!("loading cards");
            let db = Db::from_sets_dir(sets_path(), true)?;
            assert!(db.card("Dryad Arbor").ok_or(Error::NoSuchCard("Dryad Arbor".to_owned()))?.loyalty().is_none());
            let num_cards = db.into_iter().count();
            data.insert::<CardDb>(db);
            println!("{} cards loaded", num_cards);
        }
        // listen for IPC commands
        {
            thread::Builder::new().name("Lore Seeker IPC".into()).spawn(move || {
                if let Err(e) = listen_ipc(ctx_arc) { //TODO remove `if` after changing from `()` to `!`
                    eprintln!("{:?}", e);
                    notify_ipc_crash(e);
                }
            }).annotate("spawn IPC connection thread")?;
        }
        // connect to Discord
        client.start_autosharded()?;
        thread::sleep(Duration::from_secs(1)); // wait to make sure websockets can be closed cleanly
    }
    Ok(())
}
