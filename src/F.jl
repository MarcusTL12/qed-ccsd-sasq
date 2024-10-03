

# F_op(L, R) = ∑_μν L_μ <μ| e^-T [[H, op], τ_ν] e^T |HF> R_μ
function F_matrix(H, op, T, n_ph)
    F_LR = SASQ.Expression(0)

    inner_comm = commutator(H, op)

    for i in 1:(n_ph + 1), j in 1:(n_ph + 1)
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

function F0_ai(H)
    println("F0_ai =\n")
    F_matrix_wo_ccsd(H, ex_ketop(1, 2))
end

function F0_aibj(H)
    F = F_matrix_wo_ccsd(H, ex_ketop(1, 2, 3, 4))

    s, ss, ns = desymmetrize(F, make_permutation_mappings([(1, 2), (3, 4)]))

    s += 1//2 * ss

    println("F0_aibj =\n")

    s
end

function F1(H)
    println("F0 =\n")
    F_matrix_wo_ccsd(H, b')
end

function F1_ai(H)
    println("F0_ai =\n")
    F_matrix_wo_ccsd(H, ex_ketop(1, 2) * b')
end

function F1_aibj(H)
    F = F_matrix_wo_ccsd(H, ex_ketop(1, 2, 3, 4) * b')

    s, ss, ns = desymmetrize(F, make_permutation_mappings([(1, 2), (3, 4)]))

    s += 1//2 * ss

    println("F1_aibj =\n")

    s
end
