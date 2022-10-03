mod sealed { pub trait Sealed {} }
use sealed::Sealed;

pub struct Condition<const VALUE: bool>;
pub trait IsTrue: Sealed {}
pub trait IsFalse: Sealed {}

impl IsTrue for Condition<true> {}
impl IsFalse for Condition<false> {}
impl<const VALUE: bool> Sealed for Condition<VALUE> {}

macro_rules! is_trait {
    ($($trait:path as $name:ident => $fn:ident),+) => {
        $(
            pub trait $name {
                fn $fn () -> bool;
            }
            
            impl<T: ?Sized> const $name for T {
                #[inline(always)]
                default fn $fn () -> bool { return false }
            }
            
            impl<T: ?Sized + $trait> const $name for T {
                #[inline(always)]
                fn $fn () -> bool { return true }
            }
        )+
    };
}

is_trait! {
    Send as IsSend => is_send,
    Sync as IsSync => is_sync,
    core::panic::UnwindSafe as IsUnwindSafe => is_unwind_safe,
    core::panic::RefUnwindSafe as IsRefUnwindSafe => is_ref_unwind_safe
}