#![no_std]
#![feature(unboxed_closures, const_trait_impl, negative_impls, specialization)]

use marker::{IsSend, IsSync, IsUnwindSafe, IsRefUnwindSafe};
pub use typed_closures_proc::*;
use core::{alloc::Layout};
pub mod marker;

#[inline(always)]
pub const fn output_size<Args, F: FnOnce<Args>> (_: &F) -> usize {
    return core::mem::size_of::<F::Output>()
}

#[inline(always)]
pub const fn output_align<Args, F: FnOnce<Args>> (_: &F) -> usize {
    return core::mem::align_of::<F::Output>()
}

#[inline(always)]
pub const fn output_layout<Args, F: FnOnce<Args>> (_: &F) -> Layout {
    return Layout::new::<F::Output>()
}

#[inline(always)]
pub const fn output_is_send<Args, F: FnOnce<Args>> (_: &F) -> bool where F::Output: ~const IsSend {
    return <F::Output as IsSend>::is_send()
}

#[inline(always)]
pub const fn output_is_sync<Args, F: FnOnce<Args>> (_: &F) -> bool where F::Output: ~const IsSync {
    return <F::Output as IsSync>::is_sync()
}

#[inline(always)]
pub const fn output_is_unwind_safe<Args, F: FnOnce<Args>> (_: &F) -> bool where F::Output: ~const IsUnwindSafe {
    return <F::Output as IsUnwindSafe>::is_unwind_safe()
}

#[inline(always)]
pub const fn output_is_ref_unwind_safe<Args, F: FnOnce<Args>> (_: &F) -> bool where F::Output: ~const IsRefUnwindSafe {
    return <F::Output as IsRefUnwindSafe>::is_ref_unwind_safe()
}

#[inline(always)]
pub const fn cast_to_output<T: ?Sized, Args, F: FnOnce<Args>> (ptr: *const T, _: &F) -> *const F::Output {
    return ptr.cast()
}

#[inline(always)]
pub const fn cast_to_output_mut<T: ?Sized, Args, F: FnOnce<Args>> (ptr: *mut T, _: &F) -> *mut F::Output {
    return ptr.cast()
}