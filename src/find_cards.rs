//! A replacement of magic-search-engine's `find_cards` CLI frontend that uses the /list API.

#![deny(rust_2018_idioms, unused, unused_import_braces, unused_qualifications, warnings)]

use {
    std::env,
    itertools::Itertools as _,
    lore_seeker::*
};

fn main() -> Result<(), Error> {
    let (_, results) = resolve_query(None, &env::args().skip(1).join(" "))?;
    for (card_name, _) in results {
        println!("{}", card_name);
    }
    Ok(())
}
