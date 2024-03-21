
function block_noind(op)
    println("D =\n")
    op |> hf_expectation_value |> simplify_heavy
end

function block_oo(op)
    D = (op * occupied(1, 2)
         |> hf_expectation_value |> simplify_heavy)
    println("D_ij =\n")
    (D, translate(OccupiedOrbital => 1:2))
end

function block_ov(op)
    D = (op * occupied(1) * virtual(2)
         |> hf_expectation_value |> simplify_heavy)
    println("D_ia =\n")
    (D, translate(
        OccupiedOrbital => 1,
        VirtualOrbital => 2))
end

function block_vo(op)
    D = (op * occupied(2) * virtual(1)
         |> hf_expectation_value |> simplify_heavy)
    println("D_ai =\n")
    (D, translate(
        OccupiedOrbital => 2,
        VirtualOrbital => 1))
end

function block_vv(op)
    D = (op * virtual(1, 2)
         |> hf_expectation_value |> simplify_heavy)
    println("D_ab =\n")
    (D, translate(
        VirtualOrbital => 1:2))
end

function block_oooo(op)
    D = (op * occupied(1, 2, 3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_ijkl =\n")
    (D, translate(OccupiedOrbital => 1:4))
end

function block_ooov(op)
    D = (op * occupied(1, 2, 3) * virtual(4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_ijka =\n")
    (D, translate(
        OccupiedOrbital => 1:3,
        VirtualOrbital => 4))
end

function block_oovo(op)
    D = (op * occupied(1, 2, 4) * virtual(3)
         |> hf_expectation_value |> simplify_heavy)
    println("d_ijak =\n")
    (D, translate(
        OccupiedOrbital => [1, 2, 4],
        VirtualOrbital => 3))
end

function block_oovv(op)
    D = (op * occupied(1, 2) * virtual(3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_ijab =\n")
    (D, translate(
        OccupiedOrbital => 1:2,
        VirtualOrbital => 3:4))
end

function block_ovoo(op)
    D = (op * occupied(1, 3, 4) * virtual(2)
         |> hf_expectation_value |> simplify_heavy)
    println("d_iajk =\n")
    (D, translate(
        OccupiedOrbital => [1, 3, 4],
        VirtualOrbital => 2))
end

function block_ovov(op)
    D = (op * occupied(1, 3) * virtual(2, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_iajb =\n")
    (D, translate(
        OccupiedOrbital => [1, 3],
        VirtualOrbital => [2, 4]))
end

function block_ovvo(op)
    D = (op * occupied(1, 4) * virtual(2, 3)
         |> hf_expectation_value |> simplify_heavy)
    println("d_iabj =\n")
    (D, translate(
        OccupiedOrbital => [1, 4],
        VirtualOrbital => [2, 3]))
end

function block_ovvv(op)
    D = (op * occupied(1) * virtual(2, 3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_iabc =\n")
    (D, translate(
        OccupiedOrbital => 1,
        VirtualOrbital => [2, 3, 4]))
end

function block_vooo(op)
    D = (op * occupied(2, 3, 4) * virtual(1)
         |> hf_expectation_value |> simplify_heavy)
    println("d_aijk =\n")
    (D, translate(
        OccupiedOrbital => 2:4,
        VirtualOrbital => 1))
end

function block_voov(op)
    D = (op * occupied(2, 3) * virtual(1, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_aijb =\n")
    (D, translate(
        OccupiedOrbital => [2, 3],
        VirtualOrbital => [1, 4]))
end

function block_vovo(op)
    D = (op * occupied(2, 4) * virtual(1, 3)
         |> hf_expectation_value |> simplify_heavy)
    println("d_aibj =\n")
    (D, translate(
        OccupiedOrbital => [2, 4],
        VirtualOrbital => [1, 3]))
end

function block_vovv(op)
    D = (op * occupied(2) * virtual(1, 3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_aibc =\n")
    (D, translate(
        OccupiedOrbital => 2,
        VirtualOrbital => [1, 3, 4]))
end

function block_vvoo(op)
    D = (op * occupied(3, 4) * virtual(1, 2)
         |> hf_expectation_value |> simplify_heavy)
    println("d_abij =\n")
    (D, translate(
        OccupiedOrbital => 3:4,
        VirtualOrbital => 1:2))
end

function block_vvvv(op)
    D = (op * virtual(1, 2, 3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_abcd =\n")
    (D, translate(VirtualOrbital => 1:4))
end

function one_electron_blocks(op_func)
    op_func(block_oo)
    op_func(block_ov)
    op_func(block_vo)
    op_func(block_vv)
end

function two_electron_blocks(op_func)
    op_func(block_oooo)
    op_func(block_ooov)
    op_func(block_oovo)
    op_func(block_oovv)
    op_func(block_ovov)
    op_func(block_ovvo)
    op_func(block_ovvv)
    op_func(block_vovo)
    op_func(block_vovv)
    op_func(block_vvvv)
end

function ref_ref(op)
    function f(block)
        block(simplify(bch(op, T, 3)))
    end
end

function mu_ref(op, mu_ph)
    op = L_mu[mu_ph+1] * simplify(bch(op, T, 3))
    function f(block)
        block(op)
    end
end

function mu_nu(op, mu_ph, nu_ph)
    op = L_mu[mu_ph+1] * simplify(bch(op, T, 3)) * R_mu[nu_ph+1]
    function f(block)
        block(op)
    end
end
