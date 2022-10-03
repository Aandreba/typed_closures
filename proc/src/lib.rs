#![feature(unzip_option, result_option_inspect)]
mod utils;

use std::{ops::{Deref, DerefMut}, collections::hash_map::DefaultHasher, hash::{Hash, Hasher}, hint::unreachable_unchecked};
use syn::{punctuated::Punctuated, ItemTrait, TraitItem, ItemImpl, parse::Parse, Token, Attribute, ImplItem, ImplItemMethod, Receiver};
use proc_macro2::{TokenStream, Span};
use quote::{quote, format_ident, quote_spanned, ToTokens};
use syn::{parse_macro_input, ItemFn, Signature, ReturnType, spanned::Spanned, FnArg, GenericParam, Type, LifetimeDef, parse_quote};
use utils::to_pascal_case;

#[proc_macro_attribute]
pub fn sized_future (_: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let og_item = TokenStream::from(item.clone());
    let ItemFn { attrs, vis, sig, block } = parse_macro_input!(item as ItemFn);
    let Signature { constness, asyncness, unsafety, abi, fn_token, ident, mut generics, paren_token, mut inputs, variadic, output } = sig;

    // Add future lifetimes
    let mut c = 'a'..='z';
    let mut phtms = Vec::<Type>::new();

    // Add lifetimes as phantom data, add lifetimes to elided
    for arg in inputs.iter_mut() {
        if let FnArg::Typed(arg) = arg {
            if let Type::Reference(rf) = arg.ty.deref_mut() {
                if rf.lifetime.is_none() {
                    let ident = format_ident!("__{}__", c.next().expect("lifetime overflow"));
                    let lt = syn::Lifetime { apostrophe: Span::call_site(), ident };
                    generics.params.push(GenericParam::Lifetime(LifetimeDef::new(lt.clone())));
                    rf.lifetime = Some(lt);
                }
                
                phtms.push(*arg.ty.clone());
            }
        }
    }

    let (generic_impl, generic_ty, generic_where) = generics.split_for_impl();
    let name = format_ident!("{}", to_pascal_case(&ident.to_string()));
    let args = inputs.iter().map(|x| match x {
        FnArg::Receiver(_) => todo!(),
        FnArg::Typed(x) => x.pat.deref()
    });
    let output = match &output {
        ReturnType::Default => quote_spanned! { output.span() => () },
        ReturnType::Type(_, ret) => quote_spanned! { ret.span() => #ret },
    };

    macro_rules! trait_where {
        ($($var:ident => $fn:ident),+) => {
            $(
                let $var = {
                    let mut cloned = generic_where.cloned().unwrap_or_else(|| syn::WhereClause {
                        where_token: <syn::Token![where]>::default(),
                        predicates: Punctuated::new(),
                    });

                    cloned.predicates.push(parse_quote! {
                        ::typed_closures::marker::Condition<{::typed_closures::$fn(&#ident)}>: ::typed_closures::marker::IsFalse
                    });

                    cloned
                };
            )+
        };
    }
    
    trait_where! {
        send_where => output_is_send,
        sync_where => output_is_sync,
        unwind_safe_where => output_is_unwind_safe,
        ref_unwind_safe_where => output_is_ref_unwind_safe
    }

    quote! {
        #og_item

        #[repr(C)]
        #vis struct #name #generic_impl #generic_where {
            _align: <::typed_closures::marker::AlignSelector<{::typed_closures::output_align(&#ident)}> as ::typed_closures::marker::Alignment>::Equivalent,
            fut: [u8; ::typed_closures::output_size(&#ident)],
            _lt: ::core::marker::PhantomData<(#(#phtms),*)>,
            _send: ::typed_closures::marker::PhantomSend<{::typed_closures::output_is_send(&#ident)}>,
            _sync: ::typed_closures::marker::PhantomSync<{::typed_closures::output_is_sync(&#ident)}>,
            _unwind: ::typed_closures::marker::PhantomUnwind<{::typed_closures::output_is_unwind_safe(&#ident)}>,
            _ref_unwind: ::typed_closures::marker::PhantomRefUnwind<{::typed_closures::output_is_ref_unwind_safe(&#ident)}>,
            _unpin: ::core::marker::PhantomPinned
        }

        impl #generic_impl #name #generic_ty #generic_where {
            #[inline(always)]
            #vis #constness #unsafety fn new (#inputs) -> Self {
                let fut = #ident (#(#args),*);
                debug_assert_eq!(::core::alloc::Layout::for_value(&fut), ::core::alloc::Layout::new::<Self>());

                return Self {
                    _align: <::typed_closures::marker::AlignSelector<{::typed_closures::output_align(&#ident)}> as ::typed_closures::marker::Alignment>::new(),
                    fut: unsafe { ::core::mem::transmute(fut) },
                    _lt: ::core::marker::PhantomData,
                    _send: ::typed_closures::marker::PhantomSend,
                    _sync: ::typed_closures::marker::PhantomSync,
                    _unwind: ::typed_closures::marker::PhantomUnwind,
                    _ref_unwind: ::typed_closures::marker::PhantomRefUnwind,
                    _unpin: ::core::marker::PhantomPinned
                };
            }

            /// Returns the future as it's original type
            #[inline(always)]
            #vis fn into_inner (self) -> impl ::core::future::Future<Output = #output> {
                let this = ::core::mem::ManuallyDrop::new(self);
                unsafe {
                    ::core::ptr::read(
                        ::typed_closures::cast_to_output(::core::ptr::addr_of!(this).cast::<Self>(), &#ident)
                    )
                }
            }
        }

        impl #generic_impl ::core::future::Future for #name #generic_ty #generic_where {
            type Output = #output;
        
            #[inline]
            fn poll(mut self: ::core::pin::Pin<&mut Self>, cx: &mut ::core::task::Context<'_>) -> ::core::task::Poll<Self::Output> {
                unsafe {
                    let mut this = ::core::pin::Pin::into_inner_unchecked(self);
                    ::core::future::Future::poll(
                        ::core::pin::Pin::new_unchecked(
                            &mut *::typed_closures::cast_to_output_mut(::core::ptr::addr_of_mut!(this.fut), &#ident)
                        )
                    , cx)   
                }
            }
        }

        impl #generic_impl ::core::ops::Drop for #name #generic_ty #generic_where {
            #[inline(always)]
            fn drop (&mut self) {
                unsafe {
                    ::core::ptr::drop_in_place(
                        ::typed_closures::cast_to_output_mut(::core::ptr::addr_of_mut!(self.fut), &#ident)
                    )
                }
            }
        }
    }.into()
}

#[proc_macro_attribute]
pub fn async_trait (_: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    return match parse_macro_input!(item as AsyncTraitItem) {
        AsyncTraitItem::Def(item) => async_trait_def(item),
        AsyncTraitItem::Impl(item) => async_trait_impl(item)
    }
}

fn async_trait_def (mut item: ItemTrait) -> proc_macro::TokenStream {
    let mut new_types = Vec::<TraitItem>::with_capacity(1);

    for item in item.items.iter_mut() {
        if let TraitItem::Method(method) = item {
            if method.sig.asyncness.is_some() {
                let ident = format_ident!("{}", to_pascal_case(&method.sig.ident.to_string()));
                let output = match method.sig.output {
                    ReturnType::Default => parse_quote! { () },
                    ReturnType::Type(_, ref ty) => *ty.clone()
                };

                new_types.push(TraitItem::Type(parse_quote! { type #ident: ::core::future::Future<Output = #output>; }));
                method.sig.output = ReturnType::Type(Default::default(), Box::new(parse_quote! { Self::#ident }));
                method.sig.asyncness = None;
            }
        }
    }

    item.items.extend(new_types);
    item.into_token_stream().into()
}

fn async_trait_impl (mut item: ItemImpl) -> proc_macro::TokenStream {
    let self_ty = &item.self_ty;
    let mut extra = TokenStream::default();
    let mut type_impls = Vec::<ImplItem>::with_capacity(1);

    let mod_hash = {
        let mut hasher = DefaultHasher::default();
        self_ty.hash(&mut hasher);
        hasher.finish()
    };
    let mod_name = format_ident!("{}__{mod_hash}", self_ty.to_token_stream().to_string());

    for impl_item in item.items.iter_mut() {
        if let ImplItem::Method(method) = impl_item {
            if method.sig.asyncness.is_none() { continue }
            let ImplItemMethod { attrs, vis, defaultness, sig, block } = method;
            let Signature { constness, asyncness, unsafety, abi, fn_token, ident, generics, paren_token, inputs, variadic, output } = sig;

            let hash = {
                let mut hasher = DefaultHasher::default();
                item.self_ty.hash(&mut hasher);
                hasher.finish()
            };

            let args = inputs.iter().map(|arg| match arg {
                FnArg::Receiver(ty) => ty.self_token.to_token_stream(),
                FnArg::Typed(ty) => ty.pat.to_token_stream()
            }).collect::<Vec<_>>();

            let future_inputs = {
                let mut cloned = inputs.clone();
                for arg in cloned.iter_mut() {
                    if let FnArg::Receiver(_) = arg {
                        unsafe {
                            let Receiver { attrs, reference, mutability, .. } = match core::ptr::read(arg) {
                                FnArg::Receiver(recv) => recv,
                                _ => unreachable_unchecked()
                            };
                            let (and, lt) = reference.unzip();
                            let src = parse_quote! { #(#attrs)* this: #and #lt #mutability #self_ty };
                            core::ptr::write(arg, src);
                        }
                        break;
                    }
                }
                cloned
            };
            
            let target_name = format!("{}__{hash}__{}", item.self_ty.to_token_stream().to_string(), ident);
            let target = format_ident!("{}", target_name);
            let target_pascal = format_ident!("{}", to_pascal_case(&target_name));
            let output_ident = format_ident!("{}", to_pascal_case(&ident.to_string()));

            let mut generics = generics.clone();
            generics.params.extend(item.generics.params.iter().cloned());
            generics.make_where_clause().predicates.extend(item.generics.make_where_clause().predicates.iter().cloned());
            let (impl_generics, ty_generics, where_generics) = generics.split_for_impl();

            extra.extend(quote! {
                #[::typed_closures::sized_future]
                #[doc(hidden)]
                pub(super) #constness #asyncness #unsafety #abi #fn_token #target #impl_generics (#future_inputs) #output #where_generics #block
            });

            type_impls.push(ImplItem::Type(parse_quote! { type #output_ident = #target_pascal #ty_generics; }));
            sig.output = ReturnType::Type(Default::default(), Box::new(parse_quote! { Self::#output_ident }));
            sig.asyncness = None;
            method.block = parse_quote! {{
                return #target_pascal::new(#(#args),*)
            }};

            //todo!()
        }
    }

    item.items.extend(type_impls);

    quote! {
        #[doc(hidden)]
        mod #mod_name { 
            use super::*;
            #extra
        }

        #[doc(hidden)]
        pub use #mod_name::*;
        #item
    }.into()
}

enum AsyncTraitItem {
    Impl (ItemImpl),
    Def (ItemTrait)
}

impl Parse for AsyncTraitItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attrs = Vec::new();
        if input.peek(Token![#]) {
            attrs = Attribute::parse_outer(input)?
        }

        if input.peek(Token![impl]) || input.peek2(Token![impl]) || input.peek3(Token![impl]) {
            let mut v = ItemImpl::parse(input)?;
            v.attrs = attrs;
            return Ok(Self::Impl(v))
        }

        let mut v = ItemTrait::parse(input)?;
        v.attrs = attrs;
        return Ok(Self::Def(v))
    }
}