#![feature(trivial_bounds)]
use std::alloc::Layout;

use tokio::io::{stdout, AsyncWriteExt};
use typed_closures::*;

fn main () {
    let alpha = test();
    let beta = Test::new();

    println!("{:?} v. {:?}", Layout::for_value(&alpha), Layout::for_value(&beta))
}

#[sized_future]
async fn test () {
    stdout().write_all(b"hello world").await.unwrap()
}