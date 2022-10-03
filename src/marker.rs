mod sealed { pub trait Sealed {} }
use sealed::Sealed;

// ALIGNMENT
pub trait Alignment: Sealed {
    type Equivalent;
    fn new () -> Self::Equivalent;
}
pub struct AlignSelector<const ALIGN: usize>;
impl<const ALIGN: usize> Sealed for AlignSelector<ALIGN> {}

macro_rules! impl_align {
    ($($name:ident => $align:literal),+) => {
        $(
            #[repr(align($align))]
            pub struct $name;

            impl const Alignment for AlignSelector<$align> {
                type Equivalent = $name;
                
                #[inline(always)]
                fn new () -> Self::Equivalent { $name }
            }
        )+
    };
}

impl_align! {
    Align1 => 1,
    Align2 => 2,
    Align4 => 4,
    Align8 => 8,
    Align16 => 16,
    Align32 => 32,
    Align64 => 64
}

// CONDITIONS
pub struct Condition<const VALUE: bool>;
pub trait IsTrue: Sealed {}
pub trait IsFalse: Sealed {}

impl IsTrue for Condition<true> {}
impl IsFalse for Condition<false> {}
impl<const VALUE: bool> Sealed for Condition<VALUE> {}

macro_rules! is_trait {
    ($($trait:path as $phtm:ident + $name:ident => $fn:ident),+) => {
        $(
            pub struct $phtm<const V: bool>;
            impl !$trait for $phtm<false> {}

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
    Send as PhantomSend + IsSend => is_send,
    Sync as PhantomSync + IsSync => is_sync,
    core::panic::UnwindSafe as PhantomUnwind + IsUnwindSafe => is_unwind_safe,
    core::panic::RefUnwindSafe as PhantomRefUnwind + IsRefUnwindSafe => is_ref_unwind_safe
}