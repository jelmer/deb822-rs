use std::fs;

use apt_sources::{
    traits::RepositoryMut as RepositoryTrait, Repositories, Repository, RepositoryType,
};
use url::Url;

pub fn main() {
    let mut r = Repository::empty();

    r.set_types([RepositoryType::Binary]);
    r.set_uris(&[Url::parse("https://packages.distro.rs/debian").expect("Right URL")]);
    r.set_suites(vec!["stable"]);
    r.set_components(vec!["main".to_owned()]);

    let r = Repositories::new([r]);

    let txt = r.to_string();

    fs::write("test.sources", txt).expect("Unable to write file");
}
