use crate::{
    core::{
        compiler::ir::{Expr, SpecialForm},
        object::LispObject,
    },
    gc::Gc,
};

#[derive(Clone, Debug)]
pub struct MacroItem(Gc<MacroItemType>);

#[repr(align(16))]
#[derive(Clone, Debug)]
pub enum MacroItemType {
    // List(Vec<LispValue>),
    // Vector(Vec<LispValue>),
    // ignored for now
    PrintedRep(String),
}

// impl MacroItem {
//     fn from_list(src: Vec<Expr>) -> Self {
//         MacroItem::List(
//             src.into_iter()
//                 .map(From::from)
//                 .map(LispValue::MacroItem)
//                 .collect(),
//         )
//     }
// }

// impl From<Expr> for MacroItem {
//     fn from(value: Expr) -> Self {
//         todo!()
//     }
// }

// impl From<SpecialForm> for MacroItem {
//     fn from(form: SpecialForm) -> Self {
//         match form {
//             SpecialForm::And(exprs) => MacroItem::from_list(exprs),
//             SpecialForm::Catch(catch) => todo!(),
//             SpecialForm::Cond(cond) => todo!(),
//             SpecialForm::ConditionCase(condition_case) => todo!(),
//             SpecialForm::Defconst(defconst) => todo!(),
//             SpecialForm::Defvar(defvar) => todo!(),
//             SpecialForm::Function(function) => todo!(),
//             SpecialForm::If(_) => todo!(),
//             SpecialForm::Interactive(interactive) => todo!(),
//             SpecialForm::Lambda(lambda) => todo!(),
//             SpecialForm::Let(_) => todo!(),
//             SpecialForm::LetStar(let_star) => todo!(),
//             SpecialForm::Or(exprs) => todo!(),
//             SpecialForm::Prog1(prog1) => todo!(),
//             SpecialForm::Prog2(prog2) => todo!(),
//             SpecialForm::Progn(exprs) => todo!(),
//             SpecialForm::Quote(quote) => todo!(),
//             SpecialForm::SaveCurrentBuffer(save_current_buffer) => todo!(),
//             SpecialForm::SaveExcursion(save_excursion) => todo!(),
//             SpecialForm::SaveRestriction(save_restriction) => todo!(),
//             SpecialForm::Set(set) => todo!(),
//             SpecialForm::Setq(setq) => todo!(),
//             SpecialForm::SetqDefault(setq_default) => todo!(),
//             SpecialForm::UnwindProtect(unwind_protect) => todo!(),
//             SpecialForm::While(_) => todo!(),
//         }
//     }
// }
