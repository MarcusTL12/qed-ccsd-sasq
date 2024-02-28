
hF = ∑((
        real_tensor("F", 1, 2) +
        ∑((-2 // 1 * psym_tensor("g", 1, 2, 3, 3) +
           psym_tensor("g", 1, 3, 3, 2)) * occupied(3), [3])
    ) * E(1, 2) * electron(1, 2), 1:2)

HF = simplify(hF + g + H_ω + H_Eb + H_b)

He = simplify(h + g)
HFe = simplify(hF + g)

hFL = ∑((
        rsym_tensor("F", 1, 2) -
        ∑(psym_tensor("L", 1, 2, 3, 3) * occupied(3), [3])
    ) * E(1, 2), 1:2)
gL = 1 // 2 * ∑((2 * psym_tensor("L", 1, 2, 3, 4) + psym_tensor("L", 1, 4, 3, 2)) // 3 * e(1, 2, 3, 4), 1:4) |> simplify

HFL = simplify(hFL + gL)

include("jacobian_trans_right.jl")
include("jacobian_trans_left.jl")

function jacobian_element(left_op, right_op, H, n=2)
    x = SASQ.simplify_terms(left_op * bch(commutator(H, right_op), T, n))

    SASQ.simplify_terms(hf_expectation_value(x))
end

A_trans = translate(OccupiedOrbital => 2:2:10, VirtualOrbital => 1:2:10)

function A_H_photon_block(A_block)
    println("H_ω:")
    display(A_block(H_ω))
    println("\nH_b:")
    display(A_block(H_b))
    println("\nH_Eb:")
    display(A_block(H_Eb))
end

# T T

function A_t1_t1(H)
    left = deex_braop(1, 2)
    right = ex_ketop(3, 4)
    A = jacobian_element(left, right, H)
    println("A_ai,bj =")
    A, A_trans
end

function A_t1_t2(H)
    left = deex_braop(1, 2)
    right = ex_ketop(3, 4, 5, 6)
    A = jacobian_element(left, right, H)
    println("A_ai,bjck =")
    A, A_trans
end

function A_t2_t1(H)
    left = deex_braop(1, 2, 3, 4)
    right = ex_ketop(5, 6)
    A = jacobian_element(left, right, H)
    println("A_aibj,ck =")
    A, A_trans
end

function A_t2_t2(H)
    left = deex_braop(1, 2, 3, 4)
    right = ex_ketop(5, 6, 7, 8)
    A = jacobian_element(left, right, H)
    println("A_aibj,ckdl =")
    A, A_trans
end

# Gamma

function A_γ_γ(H)
    left = b
    right = b'
    A = jacobian_element(left, right, H)
    println("A =")
    A, A_trans
end

function A_γ_t1(H)
    left = b
    right = ex_ketop(1, 2)
    A = jacobian_element(left, right, H)
    println("A_ai =")
    A, A_trans
end

function A_t1_γ(H)
    left = deex_braop(1, 2)
    right = b'
    A = jacobian_element(left, right, H)
    println("A_ai =")
    A, A_trans
end

function A_γ_t2(H)
    left = b
    right = ex_ketop(1, 2, 3, 4)
    A = jacobian_element(left, right, H)
    println("A_aibj =")
    A, A_trans
end

function A_t2_γ(H)
    left = deex_braop(1, 2, 3, 4)
    right = b'
    A = jacobian_element(left, right, H)
    println("A_aibj =")
    A, A_trans
end

function A_γ_s1(H)
    left = b
    right = ex_ketop(1, 2) * b'
    A = jacobian_element(left, right, H)
    println("A_ai =")
    A, A_trans
end

function A_s1_γ(H)
    left = deex_braop(1, 2) * b
    right = b'
    A = jacobian_element(left, right, H)
    println("A_ai =")
    A, A_trans
end

function A_γ_s2(H)
    left = b
    right = ex_ketop(1, 2, 3, 4) * b'
    A = jacobian_element(left, right, H)
    println("A_aibj =")
    A, A_trans
end

function A_s2_γ(H)
    left = deex_braop(1, 2, 3, 4) * b
    right = b'
    A = jacobian_element(left, right, H)
    println("A_aibj =")
    A, A_trans
end

# S1 T

function A_s1_t1(H)
    left = deex_braop(1, 2) * b
    right = ex_ketop(3, 4)
    A = jacobian_element(left, right, H)
    println("A_ai,bj =")
    A, A_trans
end

function A_t1_s1(H)
    left = deex_braop(1, 2)
    right = ex_ketop(3, 4) * b'
    A = jacobian_element(left, right, H)
    println("A_ai,bj =")
    A, A_trans
end

function A_s1_t2(H)
    left = deex_braop(1, 2) * b
    right = ex_ketop(3, 4, 5, 6)
    A = jacobian_element(left, right, H)
    println("A_ai,bjck =")
    A, A_trans
end

function A_t2_s1(H)
    left = deex_braop(1, 2, 3, 4)
    right = ex_ketop(5, 6) * b'
    A = jacobian_element(left, right, H)
    println("A_aibj,ck =")
    A, A_trans
end

# S2 T

function A_s2_t1(H)
    left = deex_braop(1, 2, 3, 4) * b
    right = ex_ketop(5, 6)
    A = jacobian_element(left, right, H)
    println("A_aibj,ck =")
    A, A_trans
end

function A_t1_s2(H)
    left = deex_braop(1, 2)
    right = ex_ketop(3, 4, 5, 6) * b'
    A = jacobian_element(left, right, H)
    println("A_ai,bjck =")
    A, A_trans
end

function A_s2_t2(H)
    left = deex_braop(1, 2, 3, 4) * b
    right = ex_ketop(5, 6, 7, 8)
    A = jacobian_element(left, right, H)
    println("A_aibj,ckdl =")
    A, A_trans
end

function A_t2_s2(H)
    left = deex_braop(1, 2, 3, 4)
    right = ex_ketop(5, 6, 7, 8) * b'
    A = jacobian_element(left, right, H)
    println("A_aibj,ckdl =")
    A, A_trans
end

# S S

function A_s1_s1(H)
    left = deex_braop(1, 2) * b
    right = ex_ketop(3, 4) * b'
    A = jacobian_element(left, right, H)
    println("A_ai,bj =")
    A, A_trans
end

function A_s1_s2(H)
    left = deex_braop(1, 2) * b
    right = ex_ketop(3, 4, 5, 6) * b'
    A = jacobian_element(left, right, H)
    println("A_ai,bjck =")
    A, A_trans
end

function A_s2_s1(H)
    left = deex_braop(1, 2, 3, 4) * b
    right = ex_ketop(5, 6) * b'
    A = jacobian_element(left, right, H)
    println("A_aibj,ck =")
    A, A_trans
end

function A_s2_s2(H)
    left = deex_braop(1, 2, 3, 4) * b
    right = ex_ketop(5, 6, 7, 8) * b'
    A = jacobian_element(left, right, H)
    println("A_aibj,ckdl =")
    A, A_trans
end
