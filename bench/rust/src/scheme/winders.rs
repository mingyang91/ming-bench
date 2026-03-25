use super::{DwAction, Winder};

pub(crate) fn compute_wind_actions(current: &[Winder], target: &[Winder]) -> Vec<DwAction> {
    // Find common prefix by winder id.
    let common = current.iter().zip(target.iter())
        .take_while(|(a, b)| a.id == b.id)
        .count();

    let mut actions = Vec::new();

    // Unwind: run out-thunks from innermost to outermost (reverse of current[common..])
    for w in current[common..].iter().rev() {
        actions.push(DwAction::PopWinder(w.id));
        actions.push(DwAction::CallThunk(w.out_thunk.clone()));
    }

    // Rewind: run in-thunks from outermost to innermost (target[common..] in order)
    for w in &target[common..] {
        actions.push(DwAction::CallThunk(w.in_thunk.clone()));
        actions.push(DwAction::PushWinder(w.clone()));
    }

    actions
}
