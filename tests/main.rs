#![feature(trivial_bounds)]

use std::alloc::Layout;
use typed_closures::*;

#[test]
fn align () {
    println!("{}", core::mem::align_of::<String>());
    println!("{}", core::mem::align_of::<[String; 0]>());
}

#[tokio::test]
async fn main_test () {
    let a = test_fut(&f32::NAN);
    let b = TestFut::new(&f32::NAN);
    println!("{:?} v. {:?}", Layout::for_value(&a), Layout::for_value(&b))
}

#[sized_future]
async fn test_fut (x: &f32) -> bool {
    return true;
    //stdout().write_all(format!("{x}").as_bytes()).await.is_ok()
}

/*
#[async_trait]
pub trait TestTrait {
    fn reguler_fn (self) -> bool;
    async fn async_fn (self) -> bool;
}


#[async_trait]
impl TestTrait for bool {
    #[inline(always)]
    fn reguler_fn (self) -> bool {
        return self;
    }

    async fn async_fn (self) -> bool {
        return this;
    }
}
*/