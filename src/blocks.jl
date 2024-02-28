
function one_electron_density_blocks()
    op = E(1, 2)

    @time begin
        b_op = simplify(bch(op, T, 3))
        b_op_u = simplify(bch(op, T, 3))
        b_op_v = simplify(bch(op, T, 3))
    end

    D0_oo = (Λ0 * b_op * occupied(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ij = ", (D0_oo, translate(OccupiedOrbital => 1:2)), '\n')

    D0_ov = (Λ0 * b_op_u * occupied(1) * virtual(2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ia = ", (D0_ov,
            translate(OccupiedOrbital => 1, VirtualOrbital => 2)), '\n')

    D0_vo = (Λ0 * b_op * occupied(2) * virtual(1)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ai = ", (D0_vo,
            translate(OccupiedOrbital => 2, VirtualOrbital => 1)), '\n')

    D0_vv = (Λ0 * b_op * virtual(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ab = ", (D0_vv,
            translate(VirtualOrbital => 1:2)), '\n')

    println()

    D1_oo = (Λ1 * b_op * occupied(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ij = ", (D1_oo, translate(OccupiedOrbital => 1:2)), '\n')

    D1_ov = (Λ1 * b_op * occupied(1) * virtual(2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ia = ", (D1_ov,
            translate(OccupiedOrbital => 1, VirtualOrbital => 2)), '\n')

    D1_vo = (Λ1 * b_op * occupied(2) * virtual(1)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ai = ", (D1_vo,
            translate(OccupiedOrbital => 2, VirtualOrbital => 1)), '\n')

    D1_vv = (Λ1 * b_op * virtual(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ab = ", (D1_vv,
            translate(VirtualOrbital => 1:2)), '\n')

    nothing
end

function one_electron_bd_density_blocks()
    op = E(1, 2) * b'

    @time begin
        b_op = simplify(bch(op, T, 3))
        b_op_u = simplify(bch(op, Tu, 3))
    end

    D0_oo = (Λ0 * b_op * occupied(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ij = ", (D0_oo, translate(OccupiedOrbital => 1:2)), '\n')

    D0_ov = (Λ0 * b_op * occupied(1) * virtual(2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ia = ", (D0_ov,
            translate(OccupiedOrbital => 1, VirtualOrbital => 2)), '\n')

    D0_vo = (Λ0 * b_op * occupied(2) * virtual(1)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ai = ", (D0_vo,
            translate(OccupiedOrbital => 2, VirtualOrbital => 1)), '\n')

    D0_vv = (Λ0 * b_op * virtual(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ab = ", (D0_vv,
            translate(VirtualOrbital => 1:2)), '\n')

    println()

    D1_oo = (Λ1 * b_op * occupied(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ij = ", (D1_oo, translate(OccupiedOrbital => 1:2)), '\n')

    D1_ov = (Λ1 * b_op_u * occupied(1) * virtual(2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ia = ", (D1_ov,
            translate(OccupiedOrbital => 1, VirtualOrbital => 2)), '\n')

    D1_vo = (Λ1 * b_op * occupied(2) * virtual(1)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ai = ", (D1_vo,
            translate(OccupiedOrbital => 2, VirtualOrbital => 1)), '\n')

    D1_vv = (Λ1 * b_op * virtual(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ab = ", (D1_vv,
            translate(VirtualOrbital => 1:2)), '\n')

    nothing
end

function one_electron_b_density_blocks()
    op = E(1, 2) * b

    @time begin
        b_op = simplify(bch(op, T, 3))
        b_op_u = simplify(bch(op, Tu, 3))
        b_op_v = simplify(bch(op, Tv, 3))
        b_op_uv = simplify(bch(op, Tuv, 3))
    end

    D0_oo = (Λ0 * b_op * occupied(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ij = ", (D0_oo, translate(OccupiedOrbital => 1:2)), '\n')

    D0_ov = (Λ0 * b_op_uv * occupied(1) * virtual(2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ia = ", (D0_ov,
            translate(OccupiedOrbital => 1, VirtualOrbital => 2)), '\n')

    D0_vo = (Λ0 * b_op * occupied(2) * virtual(1)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ai = ", (D0_vo,
            translate(OccupiedOrbital => 2, VirtualOrbital => 1)), '\n')

    D0_vv = (Λ0 * b_op * virtual(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D0_ab = ", (D0_vv,
            translate(VirtualOrbital => 1:2)), '\n')

    println()

    D1_oo = (Λ1 * b_op * occupied(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ij = ", (D1_oo, translate(OccupiedOrbital => 1:2)), '\n')

    D1_ov = (Λ1 * b_op * occupied(1) * virtual(2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ia = ", (D1_ov,
            translate(OccupiedOrbital => 1, VirtualOrbital => 2)), '\n')

    D1_vo = (Λ1 * b_op * occupied(2) * virtual(1)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ai = ", (D1_vo,
            translate(OccupiedOrbital => 2, VirtualOrbital => 1)), '\n')

    D1_vv = (Λ1 * b_op * virtual(1, 2)
             |> hf_expectation_value |> simplify_heavy)
    println("D1_ab = ", (D1_vv,
            translate(VirtualOrbital => 1:2)), '\n')

    nothing
end

function two_electron_density_blocks(Λ)
    b_op = @time simplify(bch(e(1, 2, 3, 4), T, 2))

    d_oooo = (Λ * b_op * occupied(1, 2, 3, 4)
              |> hf_expectation_value |> simplify_heavy)
    println("d_ijkl = \n", (d_oooo, translate(OccupiedOrbital => 1:4)), '\n')

    d_ooov = (Λ * b_op * occupied(1, 2, 3) * virtual(4)
              |> hf_expectation_value |> simplify_heavy)
    println("d_ijka = \n", (d_ooov,
            translate(OccupiedOrbital => 1:3, VirtualOrbital => 4)), '\n')

    d_oovo = (Λ * b_op * occupied(1, 2, 4) * virtual(3)
              |> hf_expectation_value |> simplify_heavy)
    println("d_ijak = \n", (d_oovo,
            translate(OccupiedOrbital => (1, 2, 4), VirtualOrbital => 3)), '\n')

    d_oovv = (Λ * b_op * occupied(1, 2) * virtual(3, 4)
              |> hf_expectation_value |> simplify_heavy)
    println("d_ijab = \n", (d_oovv,
            translate(OccupiedOrbital => 1:2, VirtualOrbital => 3:4)), '\n')

    # d_ooov
    # d_ovoo = (Λ * b_op * occupied(1, 3, 4) * virtual(2)
    #            |> hf_expectation_value |> simplify_heavy)
    # println("d_iajk = \n", (d_ovoo,
    #         translate(OccupiedOrbital => (1, 3, 4), VirtualOrbital => 2)),
    #     '\n')

    d_ovov = (Λ * b_op * occupied(1, 3) * virtual(2, 4)
              |> hf_expectation_value |> simplify_heavy)
    println("d_iajb = \n", (d_ovov,
            translate(OccupiedOrbital => (1, 3), VirtualOrbital => (2, 4))),
        '\n')

    d_ovvo = (Λ * b_op * occupied(1, 4) * virtual(2, 3)
              |> hf_expectation_value |> simplify_heavy)
    println("d_iabj = \n", (d_ovvo,
            translate(OccupiedOrbital => (1, 4), VirtualOrbital => 2:3)), '\n')

    d_ovvv = (Λ * b_op * occupied(1) * virtual(2, 3, 4)
              |> hf_expectation_value |> simplify_heavy)
    println("d_iabc = \n", (d_ovvv,
            translate(OccupiedOrbital => 1, VirtualOrbital => 2:4)), '\n')

    # d_oovo
    # d_vooo = (Λ * b_op * occupied(2, 3, 4) * virtual(1)
    #            |> hf_expectation_value |> simplify_heavy)
    # println("d_aijk = \n", (d_vooo,
    #         translate(OccupiedOrbital => 2:4, VirtualOrbital => 1)), '\n')

    # d_ovvo
    # d_voov = (Λ * b_op * occupied(2, 3) * virtual(1, 4)
    #            |> hf_expectation_value |> simplify_heavy)
    # println("d_iabj = \n", (d_voov,
    #         translate(OccupiedOrbital => 2:3, VirtualOrbital => (1, 4))),
    #     '\n')

    d_vovo = (Λ * b_op * occupied(2, 4) * virtual(1, 3)
              |> hf_expectation_value |> simplify_heavy)
    println("d_aibj = \n", (d_vovo,
            translate(OccupiedOrbital => (2, 4), VirtualOrbital => (1, 3))),
        '\n')

    d_vovv = (Λ * b_op * occupied(2) * virtual(1, 3, 4)
              |> hf_expectation_value |> simplify_heavy)
    println("d_aibc = \n", (d_vovv,
            translate(OccupiedOrbital => 2, VirtualOrbital => (1, 3, 4))),
        '\n')

    # d_oovv
    # d_vvoo = (Λ * b_op * occupied(3, 4) * virtual(1, 2)
    #            |> hf_expectation_value |> simplify_heavy)
    # println("d_abij = \n", (d_vvoo,
    #         translate(OccupiedOrbital => 3:4, VirtualOrbital => 1:2)),
    #     '\n')

    # ovvv
    # d_vvov = (Λ * b_op * occupied(3) * virtual(1, 2, 4)
    #           |> hf_expectation_value |> simplify_heavy)
    # println("d_abic = \n", (d_vvov,
    #         translate(OccupiedOrbital => 3, VirtualOrbital => (1, 2, 4))),
    #     '\n')

    # vovv
    # d_vvvo = (Λ * b_op * occupied(4) * virtual(1, 2, 3)
    #           |> hf_expectation_value |> simplify_heavy)
    # println("d_abci = \n", (d_vvvo,
    #         translate(OccupiedOrbital => 4, VirtualOrbital => 1:3)),
    #     '\n')

    d_vvvv = (Λ * b_op * virtual(1, 2, 3, 4)
              |> hf_expectation_value |> simplify_heavy)
    println("d_abcd = \n", (d_vvvv,
            translate(VirtualOrbital => 1:4)),
        '\n')

    nothing
end
