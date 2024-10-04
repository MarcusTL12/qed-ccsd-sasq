

# F_op(L, R) = ∑_μν L_μ <μ| e^-T [[H, op], τ_ν] e^T |HF> R_μ
function F_matrix(H, op, T, n_ph)
    F_LR = SASQ.Expression(0)

    inner_comm = commutator(H, op)

    for i in 1:(n_ph+1), j in 1:(n_ph+1)
        comm = commutator(inner_comm, R_mu[j])

        x = act_eT_on_bra(L_mu[i], -T)
        x = act_on_bra(x * comm) |> simplify
        x = act_eT_on_bra(x, T; max_ops=0)

        F_LR += simplify_heavy(x)
    end

    F_LR

    F_LR = look_for_tensor_replacements_smart(F_LR, make_exchange_transformer("t", "u"))
    F_LR = look_for_tensor_replacements_smart(F_LR, make_exchange_transformer("s", "v"))
    F_LR = look_for_tensor_replacements_smart(F_LR, make_exchange_transformer("g", "L"))
    F_LR = look_for_tensor_replacements_smart(F_LR, make_exchange_transformer("Rt", "Ru"))
    F_LR = look_for_tensor_replacements_smart(F_LR, make_exchange_transformer("Rs", "Rv"))
    F_LR = look_for_tensor_replacements_smart(F_LR, make_exchange_transformer("Lt", "Lu"))
    F_LR = look_for_tensor_replacements_smart(F_LR, make_exchange_transformer("Ls", "Lv"))

    F_LR
end

function F_matrix_wo_ccsd(H, op)
    F_full = @time F_matrix(H, op, T, 1)
    F_ccsd = @time F_matrix(H, op, T2, 0)

    F_full - F_ccsd
end

function F0_ai(H, name)
    println("F0_ai =\n")
    ex = F_matrix_wo_ccsd(H, ex_ketop(1, 2))

    println(ex)

    make_F_code(ex, "rho", "F_matrix_transformation_$(name)_singles",
        [1, 2], nothing)
end

function F0_aibj(H, name)
    F = F_matrix_wo_ccsd(H, ex_ketop(1, 2, 3, 4))

    per_map = make_permutation_mappings([(1, 2), (3, 4)])

    s, ss, ns = desymmetrize(F, per_map)

    s += 1 // 2 * ss

    println("F0_aibj =\n")

    println(s)

    make_F_code(s, "rho", "F_matrix_transformation_$(name)_doubles",
        [1, 2, 3, 4], flatten_perms(per_map))
end

function F1(H, name)
    println("F1 =\n")
    ex = F_matrix_wo_ccsd(H, b')

    println(ex)

    make_F_code(ex, "rho", "F_matrix_transformation_$(name)_photon",
        Int[], nothing)
end

function F1_ai(H, name)
    println("F1_ai =\n")
    ex = F_matrix_wo_ccsd(H, ex_ketop(1, 2) * b')

    println(ex)

    make_F_code(ex, "rho", "F_matrix_transformation_$(name)_singles_photon",
        [1, 2], nothing)
end

function F1_aibj(H, name)
    F = F_matrix_wo_ccsd(H, ex_ketop(1, 2, 3, 4) * b')

    per_map = make_permutation_mappings([(1, 2), (3, 4)])

    s, ss, ns = desymmetrize(F, per_map)

    s += 1 // 2 * ss

    println("F1_aibj =\n")

    println(s)

    make_F_code(s, "rho", "F_matrix_transformation_$(name)_doubles_photon",
        [1, 2, 3, 4], flatten_perms(per_map))
end

function make_F_code(ex, symbol, prefix, indices, outperms)
    open("$prefix.jl", "w") do io
        println(io, print_eT_function_generator(prefix, ex, symbol, indices,
            A_trans, "qed_ccsd", Dict([
                "γ₁" => "s0",
            ]), String[], outperms))
    end
end
