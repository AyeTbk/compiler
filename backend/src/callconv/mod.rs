use crate::procedure::RegisterId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallingConventionId {
    AllStack,
    SysV,
}

pub struct CallingConvention {
    pub id: CallingConventionId,
    pub integer_parameter_registers: Vec<RegisterId>,
    pub integer_return_register: RegisterId,
    pub preserved_registers: Vec<RegisterId>,
    pub scratch_registers: Vec<RegisterId>,
}

impl CallingConvention {
    pub fn register_distributor(&self) -> RegisterDistributor {
        let make_distributable_vec = |v: Vec<RegisterId>| {
            v.into_iter()
                .map(DistributableRegister::new)
                .collect::<Vec<_>>()
        };

        let integer_parameter_registers =
            make_distributable_vec(self.integer_parameter_registers.clone());
        let integer_return_register = DistributableRegister::new(self.integer_return_register);
        let preserved_registers = make_distributable_vec(self.preserved_registers.clone());
        let scratch_registers = make_distributable_vec(self.scratch_registers.clone());

        RegisterDistributor {
            integer_parameter_registers,
            integer_return_register,
            preserved_registers,
            scratch_registers,
        }
    }
}

pub struct RegisterDistributor {
    integer_parameter_registers: Vec<DistributableRegister>,
    integer_return_register: DistributableRegister,
    preserved_registers: Vec<DistributableRegister>,
    scratch_registers: Vec<DistributableRegister>,
}

impl RegisterDistributor {
    pub fn give_register(&mut self, reg: RegisterId) {
        assert!(!self.register_available(reg), "reg: {}", reg);
        self.set_register_availability(reg, true);
    }

    pub fn take_register(&mut self, reg: RegisterId) {
        assert!(self.register_available(reg), "reg: {}", reg);
        self.set_register_availability(reg, false);
    }

    pub fn take_integer_parameter_register(&mut self) -> Option<RegisterId> {
        let reg = self
            .integer_parameter_registers
            .iter()
            .find(|dr| dr.available)
            .map(|dr| dr.id);
        if let Some(reg) = reg {
            self.set_register_availability(reg, false);
        }
        reg
    }

    pub fn take_integer_return_register(&mut self) -> Option<RegisterId> {
        let dr = self.integer_return_register;
        if dr.available {
            self.set_register_availability(dr.id, false);
            Some(dr.id)
        } else {
            None
        }
    }

    pub fn take_preserved_register(&mut self) -> Option<RegisterId> {
        let reg = self
            .preserved_registers
            .iter()
            .find(|dr| dr.available)
            .map(|dr| dr.id);
        if let Some(reg) = reg {
            self.set_register_availability(reg, false);
        }
        reg
    }

    pub fn take_scratch_register(&mut self) -> Option<RegisterId> {
        let reg = self
            .scratch_registers
            .iter()
            .find(|dr| dr.available)
            .map(|dr| dr.id);
        if let Some(reg) = reg {
            self.set_register_availability(reg, false);
        }
        reg
    }

    fn set_register_availability(&mut self, reg: RegisterId, available: bool) {
        let reg_iter = self
            .integer_parameter_registers
            .iter_mut()
            .chain(Some(&mut self.integer_return_register).into_iter())
            .chain(self.preserved_registers.iter_mut())
            .chain(self.scratch_registers.iter_mut())
            .filter(|dr| dr.id == reg);

        for distributable_reg in reg_iter {
            distributable_reg.available = available;
        }
    }

    fn register_available(&mut self, reg: RegisterId) -> bool {
        let mut reg_iter = self
            .integer_parameter_registers
            .iter()
            .chain(Some(&self.integer_return_register).into_iter())
            .chain(self.preserved_registers.iter())
            .chain(self.scratch_registers.iter())
            .filter(|dr| dr.id == reg);

        reg_iter.next().map(|dr| dr.available).unwrap()
    }
}

#[derive(Clone, Copy)]
struct DistributableRegister {
    pub id: RegisterId,
    pub available: bool,
}

impl DistributableRegister {
    pub fn new(id: RegisterId) -> Self {
        Self {
            id,
            available: true,
        }
    }
}
