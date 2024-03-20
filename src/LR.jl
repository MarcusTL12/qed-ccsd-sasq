using Revise
using SpinAdaptedSecondQuantization

b = boson()

include("LR_blocks.jl")
include("W.jl")
include("omega.jl")

t(inds...) = psym_tensor("t", inds...)
s1(inds...) = psym_tensor("s₁", inds...)
s2(inds...) = psym_tensor("s₂", inds...)

L(n, inds...) = psym_tensor("L$n", inds...)
R(n, inds...) = psym_tensor("R$n", inds...)

T1 = ∑(t(1, 2) * E(1, 2) * virtual(1) * occupied(2), 1:2)
T2 = 1 // 2 * ∑(
    t(1:4...) * E(1, 2) * E(3, 4) * virtual(1, 3) * occupied(2, 4),
    1:4
)

S1_1 = ∑(s1(1, 2) * E(1, 2) * b' * virtual(1) * occupied(2), 1:2)
S2_1 = 1 // 2 * ∑(
    s1(1:4...) * E(1, 2) * E(3, 4) * b' * virtual(1, 3) * occupied(2, 4),
    1:4
)

S1_2 = ∑(s2(1, 2) * E(1, 2) * b'^2 * virtual(1) * occupied(2), 1:2)
S2_2 = 1 // 2 * ∑(
    s2(1:4...) * E(1, 2) * E(3, 4) * b'^2 * virtual(1, 3) * occupied(2, 4),
    1:4
)

Γ1 = real_tensor("γ₁") * b'
Γ2 = real_tensor("γ₂") * b'^2

T = T2 + S1_1 + S2_1 + Γ1 + S1_2 + S2_2 + Γ2
T_qed_ccsd_1 = T2 + S1_1 + S2_1 + Γ1

ex_ketop(a, i) = E(a, i) * occupied(i) * virtual(a)
ex_ketop(a, i, b, j) = E(a, i) * E(b, j) * occupied(i, j) * virtual(a, b)

deex_braop(a, i) = 1 // 2 * ex_ketop(a, i)'
deex_braop(a, i, b, j) = 1 // 3 * ex_ketop(a, i, b, j)' +
                         1 // 6 * ex_ketop(a, j, b, i)'

include("jacobian.jl")

L0_mu = ∑(L("t", 1, 2) * deex_braop(1, 2), 1:2) +
        1 // 2 * ∑(L("t", 1, 2, 3, 4) * deex_braop(1, 2, 3, 4), 1:4)
L1_mu = (real_tensor("Ls1") +
         ∑(L("s", 1, 2) * deex_braop(1, 2), 1:2) +
         1 // 2 * ∑(L("s", 1, 2, 3, 4) * deex_braop(1, 2, 3, 4), 1:4)) * b
L2_mu = (real_tensor("Ls2") * b * b)

L_mu = (L0_mu, L1_mu, L2_mu)

R0_mu = ∑(R("t", 1, 2) * ex_ketop(1, 2), 1:2) +
        1 // 2 * ∑(R("t", 1, 2, 3, 4) * ex_ketop(1, 2, 3, 4), 1:4)
R1_mu = (real_tensor("Rs") + ∑(R("s", 1, 2) * ex_ketop(1, 2), 1:2) +
         1 // 2 * ∑(R("s", 1, 2, 3, 4) * ex_ketop(1, 2, 3, 4), 1:4)) * b'

R_mu = (R0_mu, R1_mu)

function filter_nongamma2((ex, trans))
    ts = [t for t in ex.terms if any(tens.symbol ∈ ("γ₂", "Ls2") for tens in t.tensors)]
    if !isempty(ts)
        SASQ.Expression(ts), trans
    else
        0
    end
end
