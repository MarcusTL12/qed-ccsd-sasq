using SpinAdaptedSecondQuantization

# include("project.jl")
include("blocks.jl")
include("energy.jl")

t(inds...) = psym_tensor("t", inds...)
tl(inds...) = psym_tensor("ᴸt", inds...)
tt(inds...) = psym_tensor("ᵗt", inds...)

s(inds...) = psym_tensor("s", inds...)
sl(inds...) = psym_tensor("ᴸs", inds...)
st(inds...) = psym_tensor("ᵗs", inds...)

u(inds...) = psym_tensor("u", inds...)
v(inds...) = psym_tensor("v", inds...)

b = boson()

T1 = ∑(t(1, 2) * E(1, 2) * virtual(1) * occupied(2), 1:2)
T2 = 1 // 2 * ∑(
    t(1:4...) * E(1, 2) * E(3, 4) * virtual(1, 3) * occupied(2, 4),
    1:4
)

U2 = ∑(
    (1 // 3 * u(1, 2, 3, 4) + 1 // 6 * u(1, 4, 3, 2)) *
    E(1, 2) * E(3, 4) * virtual(1, 3) * occupied(2, 4),
    1:4
)

S1 = ∑(s(1, 2) * E(1, 2) * b' * virtual(1) * occupied(2), 1:2)
S2 = 1 // 2 * ∑(
    s(1:4...) * E(1, 2) * E(3, 4) * b' * virtual(1, 3) * occupied(2, 4),
    1:4
)

V2 = ∑(
    (1 // 3 * v(1, 2, 3, 4) + 1 // 6 * v(1, 4, 3, 2)) *
    E(1, 2) * E(3, 4) * b' * virtual(1, 3) * occupied(2, 4),
    1:4
)

Γ1 = real_tensor("γ") * b'

Γ2 = real_tensor("γ₂") * b' * b'

# Γ = Γ1 + Γ2
Γ = Γ1

# T = T1 + T2 + S1 + S2 + Γ
T = T2 + S1 + S2 + Γ
Tu = U2 + S1 + S2 + Γ
Tv = T2 + S1 + V2 + Γ
Tuv = U2 + S1 + V2 + Γ

h = ∑(real_tensor("h", 1, 2) * E(1, 2), 1:2)
g = 1 // 2 * ∑(psym_tensor("g", 1, 2, 3, 4) * e(1, 2, 3, 4), 1:4) |> simplify

H_ω = real_tensor("ω") * b' * b
H_Eb = ∑(real_tensor("d", 1, 2) * E(1, 2) * (b' + b), 1:2)
H_b = b' + b

ex_ketop(a, i) = E(a, i) * occupied(i) * virtual(a)
ex_ketop(a, i, b, j) = E(a, i) * E(b, j) * occupied(i, j) * virtual(a, b)

deex_braop(a, i) = 1 // 2 * ex_ketop(a, i)'
deex_braop(a, i, b, j) = 1 // 3 * ex_ketop(a, i, b, j)' +
                         1 // 6 * ex_ketop(a, j, b, i)'


Λ0 = 1 +
     ∑(tl(1, 2) * deex_braop(1, 2), 1:2) +
     1 // 2 * ∑(tt(1, 2, 3, 4) * deex_braop(1, 2, 3, 4), 1:4)

Λ1 = (real_tensor("ᴸγ") +
      ∑(sl(1, 2) * deex_braop(1, 2), 1:2) +
      1 // 2 * ∑(st(1, 2, 3, 4) * deex_braop(1, 2, 3, 4), 1:4)) * b
#   real_tensor("γᴸ₂") * b * b

Λ = Λ0 + Λ1

function get_densities(op)
    D0 = hf_expectation_value(
        Λ0 * bch(op, T, 2)
    ) |> simplify_heavy

    @show D0 length(D0.terms)
    println()

    D1 = hf_expectation_value(
        Λ1 * bch(op, T, 2)
    ) |> simplify_heavy

    @show D1 length(D1.terms)
    println()
end

# Calculate ⟨Λ| b† b |CC⟩
function photon_density()
    get_densities(b'b)
end

# Calculate ⟨Λ| b† + b |CC⟩
function photon_density_2()
    get_densities(b' + b)
end

function one_electron_density()
    get_densities(E(1, 2))
end

function two_electron_density()
    get_densities(e(1, 2, 3, 4))
end

function one_electron_one_photon_density()
    get_densities(E(1, 2) * (b' + b))
end
