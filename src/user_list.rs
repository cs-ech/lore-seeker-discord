//! Helper functions for maintaining a list of known Discord users on disk, which is used by the website to display usernames.

use std::{
    fs::{
        self,
        File
    },
    io::{
        self,
        prelude::*
    },
    path::Path
};
use serde_json::json;
use serenity::model::{
    guild::Member,
    id::{
        GuildId,
        UserId
    }
};
use crate::Error;

const PROFILES_DIR: &'static str = "/var/www/loreseeker.fenhl.net/profiles";

/// Add a Discord account to the given guild's user list.
pub fn add(guild_id: GuildId, member: Member) -> Result<(), Error> {
    let guild_dir = Path::new(PROFILES_DIR).join(guild_id.to_string());
    if !guild_dir.exists() {
        fs::create_dir(&guild_dir)?;
    }
    let user = member.user.read().clone();
    let mut f = File::create(guild_dir.join(format!("{}.json", user.id)))?;
    write!(f, "{:#}", json!({
        "discriminator": user.discriminator,
        "snowflake": user.id,
        "username": user.name
    }))?;
    Ok(())
}

/// Remove a Discord account from the given guild's user list.
pub fn remove<U: Into<UserId>>(guild_id: GuildId, user: U) -> io::Result<()> {
    match fs::remove_file(Path::new(PROFILES_DIR).join(guild_id.to_string()).join(format!("{}.json", user.into()))) {
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
        r => r
    }
}

/// (Re)initialize the given guild's user list.
pub fn set_guild<I: IntoIterator<Item=Member>>(guild_id: GuildId, members: I) -> Result<(), Error> {
    let guild_dir = Path::new(PROFILES_DIR).join(guild_id.to_string());
    if guild_dir.exists() {
        for entry in fs::read_dir(guild_dir)? {
            fs::remove_file(entry?.path())?;
        }
    }
    for member in members.into_iter() {
        add(guild_id, member)?;
    }
    Ok(())
}

/// Update the data for a guild member. Equivalent to `remove` followed by `add`.
pub fn update(guild_id: GuildId, member: Member) -> Result<(), Error> {
    remove(guild_id, &member)?;
    add(guild_id, member)?;
    Ok(())
}
