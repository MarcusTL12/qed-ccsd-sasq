
# W_pq = ∑_rs(d_pqrs LJ_rs)
# d_pqrs = ⟨L| e^-T e_pqrs e^T |R⟩

function W_op()
    simplify(∑(
        e(1, 2, 3, 4) * real_tensor("LJ", 3, 4) * electron(1, 2, 3, 4), 3:4
    ))
end

function W(op)
    x = act_eT_on_bra(L_mu_full, -T)
    x = act_on_bra(x * op) |> simplify
    x = act_eT_on_bra(x, T; max_ops=3)
    x = act_on_bra(x * R_mu_full, 0)

    x = simplify_heavy(x)

    x = look_for_tensor_replacements_smart(x, make_exchange_transformer("t", "u"))
    x = look_for_tensor_replacements_smart(x, make_exchange_transformer("s", "v"))
    x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Rt", "Ru"))
    x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Rs", "Rv"))
    x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Lt", "Lu"))
    x = look_for_tensor_replacements_smart(x, make_exchange_transformer("Ls", "Lv"))

    x
end

function W_oo()
    W(W_op() * occupied(1, 2))
end

function W_ov()
    W(W_op() * occupied(1) * virtual(2))
end

function W_vo()
    W(W_op() * occupied(2) * virtual(1))
end

function W_vv()
    W(W_op() * virtual(1, 2))
end
