#![warn(trivial_casts)]
#![deny(unused)]
#![forbid(unused_extern_crates, unused_import_braces)]

extern crate serenity;
extern crate urlencoding;

use std::env;
use std::process::{self, Stdio};

use serenity::prelude::*;
use serenity::model::{Message, Permissions, Ready};

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
            msg.channel_id.broadcast_typing().expect("failed to broadcast typing");
            let process::Output { status, stdout, .. } = process::Command::new("find_cards")
                .arg(if query == "-v" { "(-v)" } else { query })
                .stdout(Stdio::piped())
                .stderr(Stdio::null())
                .output()
                .expect("failed to execute find_cards");
            if !status.success() {
                match status.code() {
                    Some(code) => { panic!("find_cards exited with status code {}", code); }
                    None => { panic!("find_cards terminated by signal"); }
                }
            }
            let output = String::from_utf8(stdout).expect("find_cards did not output valid Unicode");
            let mut matches = output.lines();
            match (matches.next(), matches.next()) {
                (Some(_), Some(_)) => { msg.reply(&format!("{} cards found: https://loreseeker.fenhl.net/card?q={}", 2 + matches.count(), urlencoding::encode(query))).expect("failed to reply"); }
                (Some(card_name), None) => { msg.reply(&format!("{} https://loreseeker.fenhl.net/card?q=!{}", card_name, urlencoding::encode(card_name))).expect("failed to reply"); } //TODO reply with card stats & resolved Lore Seeker URL
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
