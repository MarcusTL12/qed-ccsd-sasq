
function block_oo(op)
    D = (op * occupied(1, 2)
         |> hf_expectation_value |> simplify_heavy)
    println("D_ij =\n")
    D
end

function block_ov(op)
    D = (op * occupied(1) * virtual(2)
         |> hf_expectation_value |> simplify_heavy)
    println("D_ia =\n")
    D
end

function block_vo(op)
    D = (op * occupied(2) * virtual(1)
         |> hf_expectation_value |> simplify_heavy)
    println("D_ai =\n")
    D
end

function block_vv(op)
    D = (op * virtual(1, 2)
         |> hf_expectation_value |> simplify_heavy)
    println("D_ab =\n")
    D
end

function block_oooo(op)
    D = (op * occupied(1, 2, 3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_ijkl =\n")
    D
end

function block_ooov(op)
    D = (op * occupied(1, 2, 3) * virtual(4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_ijka =\n")
    D
end

function block_oovo(op)
    D = (op * occupied(1, 2, 4) * virtual(3)
         |> hf_expectation_value |> simplify_heavy)
    println("d_ijak =\n")
    D
end

function block_oovv(op)
    D = (op * occupied(1, 2) * virtual(3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_ijab =\n")
    D
end

function block_ovoo(op)
    D = (op * occupied(1, 3, 4) * virtual(2)
         |> hf_expectation_value |> simplify_heavy)
    println("d_iajk =\n")
    D
end

function block_ovov(op)
    D = (op * occupied(1, 3) * virtual(2, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_iajb =\n")
    D
end

function block_ovvo(op)
    D = (op * occupied(1, 4) * virtual(2, 3)
         |> hf_expectation_value |> simplify_heavy)
    println("d_iabj =\n")
    D
end

function block_ovvv(op)
    D = (op * occupied(1) * virtual(2, 3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_iabc =\n")
    D
end

function block_vooo(op)
    D = (op * occupied(2, 3, 4) * virtual(1)
         |> hf_expectation_value |> simplify_heavy)
    println("d_aijk =\n")
    D
end

function block_voov(op)
    D = (op * occupied(2, 3) * virtual(1, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_aijb =\n")
    D
end

function block_vovo(op)
    D = (op * occupied(2, 4) * virtual(1, 3)
         |> hf_expectation_value |> simplify_heavy)
    println("d_aibj =\n")
    D
end

function block_vovv(op)
    D = (op * occupied(2) * virtual(1, 3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_aibc =\n")
    D
end

function block_vvoo(op)
    D = (op * occupied(3, 4) * virtual(1, 2)
         |> hf_expectation_value |> simplify_heavy)
    println("d_abij =\n")
    D
end

function block_vvvv(op)
    D = (op * virtual(1, 2, 3, 4)
         |> hf_expectation_value |> simplify_heavy)
    println("d_abcd =\n")
    D
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
        x = block(simplify(bch(op, T, 3)))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("t", "u"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("s", "v"))
        x
    end
end

function mu_ref(op, mu_ph)
    op = L_mu[mu_ph+1] * simplify(bch(op, T, 3))
    function f(block)
        x = block(op)
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("t", "u"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("s", "v"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Lt", "Lu"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Ls", "Lv"))
        x
    end
end

function mu_nu(op, mu_ph, nu_ph)
    op = L_mu[mu_ph+1] * simplify(bch(op, T, 3)) * R_mu[nu_ph+1]
    function f(block)
        x = block(op)
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("t", "u"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("s", "v"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Lt", "Lu"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Ls", "Lv"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Rt", "Ru"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Rs", "Rv"))
        x
    end
end

function mu_nu_all_photons(op, block)
    sum(mu_nu(op, i, j)(block) for i in 0:1, j in 0:1)
end

function ref_nu(op, nu_ph)
    op = simplify(bch(op, T, 3)) * R_mu[nu_ph+1]
    function f(block)
        x = block(op)
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("t", "u"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("s", "v"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Rt", "Ru"))
        x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Rs", "Rv"))
        x
    end
end

function full_gs_density(op, block)
    ref_ref(op)(block) + mu_ref(op, 0)(block) + mu_ref(op, 1)(block)
end

latex_rep = Dict([
    "Lt" => "\\bar{t}",
    "Ls" => "\\bar{s}",
    "Lu" => "\\bar{u}",
    "Lv" => "\\bar{v}",
    "Ls1" => "\\bar{\\gamma}",
    "γ₁" => "\\gamma"
])

function D_oo_separated()
    D = mu_nu_all_photons(E(1, 2) * (b' + b), block_oo)

    diag_terms = eltype(D.terms)[]
    other_terms = eltype(D.terms)[]

    for t in D.terms
        if length(t.deltas) == 1
            empty!(t.deltas)
            delete!(t.constraints, 1)
            delete!(t.constraints, 2)
            push!(diag_terms, t)
        else
            push!(other_terms, t)
        end
    end

    diag_ex = SASQ.Expression(diag_terms) |> separate_LJ_ii
    other_ex = SASQ.Expression(other_terms) |> separate_LJ_ii

    trans = translate(OccupiedOrbital => 1:2)

    make_W_code(diag_ex, "D", "density_1e_1b_qed_ccsd_mu_nu_oo_diagonal_terms_qed_ccsd", Int[], trans)
    make_W_code(other_ex, "D_oo", "density_1e_1b_qed_ccsd_mu_nu_oo_qed_ccsd", [1, 2], trans)
end

function D_vo()
    D = mu_nu_all_photons(E(1, 2) * (b' + b), block_vo)

    trans = translate(OccupiedOrbital => 1:2)

    make_W_code(D, "D_vo", "density_1e_1b_qed_ccsd_mu_nu_vo_qed_ccsd", [1, 2], trans)
end

function D_vo()
    D = mu_nu_all_photons(E(1, 2) * (b' + b), block_vo)

    trans = translate(OccupiedOrbital => [2], VirtualOrbital => [1])

    make_W_code(D, "D_vo", "density_1e_1b_qed_ccsd_mu_nu_vo_qed_ccsd", [1, 2], trans)
end

function D_ov()
    D = mu_nu_all_photons(E(1, 2) * (b' + b), block_ov)

    trans = translate(OccupiedOrbital => [1], VirtualOrbital => [2])

    make_W_code(D, "D_ov", "density_1e_1b_qed_ccsd_mu_nu_ov_qed_ccsd", [1, 2], trans)
end

function D_vv()
    D = mu_nu_all_photons(E(1, 2) * (b' + b), block_vv)

    trans = translate(VirtualOrbital => 1:2)

    make_W_code(D, "D_vv", "density_1e_1b_qed_ccsd_mu_nu_vv_qed_ccsd", [1, 2], trans)
end
