import SciLean

open SciLean


def H (m k : ℝ) (x p : ℝ^{2}) := (1/(2*m)) * ∥p∥² + k/2 * ∥x∥²

approx solver (m k : ℝ) (steps : Nat)
  := (ode_solve (HamiltonianSystem (H m k)))
by
  -- Unfold Hamiltonian definition and compute gradients
  unfold HamiltonianSystem
  unfold H
  simp [hold]

  -- Apply RK4 method
  rw [ode_solve_fixed_dt runge_kutta4_step]
  approx_limit steps; simp; intro steps';


