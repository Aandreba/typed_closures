#![feature(trivial_bounds)]

use tokio::io::{AsyncWriteExt, stdout};
use typed_closures::*;

/*#[tokio::test]
async fn main_test () {
    let a = test_fut(&f32::NAN);
    let b = TestFut::new(&f32::NAN);
    //println!("{} v. {}", a.await, b.await)
}

#[sized_future]
async fn test_fut (x: &f32) -> bool {
    return true;
    //stdout().write_all(format!("{x}").as_bytes()).await.is_ok()
}*/

#[async_trait]
pub trait TestTrait {
    fn reguler_fn (x: f32) -> bool;
    async fn async_fn (x: f32) -> bool;
}

#[async_trait]
impl TestTrait for bool {
    #[inline(always)]
    fn reguler_fn (x: f32) -> bool {
        return false;
    }

    async fn async_fn (x: f32) -> bool {
        return false;
    }
}