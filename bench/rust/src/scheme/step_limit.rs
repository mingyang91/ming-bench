use std::cell::Cell;
use std::rc::Rc;

use super::error::{EvalError, SourcePos};

pub(super) type StepBudgetRef = Rc<StepBudget>;

pub(super) struct StepBudget {
    max_steps: Option<usize>,
    remaining: Cell<usize>,
}

impl StepBudget {
    pub(super) fn unlimited() -> StepBudgetRef {
        Rc::new(Self {
            max_steps: None,
            remaining: Cell::new(0),
        })
    }

    pub(super) fn limited(max_steps: usize) -> StepBudgetRef {
        Rc::new(Self {
            max_steps: Some(max_steps),
            remaining: Cell::new(max_steps),
        })
    }

    pub(super) fn step(&self, position: SourcePos) -> Result<(), EvalError> {
        let Some(max_steps) = self.max_steps else {
            return Ok(());
        };

        let remaining = self.remaining.get();
        if remaining == 0 {
            return Err(EvalError::StepLimitExceeded { max_steps }.with_position(position));
        }

        self.remaining.set(remaining - 1);
        Ok(())
    }
}
