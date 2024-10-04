
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

function separate_LJ_ii(ex)
    terms_with = eltype(ex.terms)[]
    terms_without = eltype(ex.terms)[]

    for t in ex.terms
        i = findfirst(SASQ.get_symbol(tens) == "LJ" for tens in t.tensors)

        pushed = false

        if !isnothing(i)
            tens = t.tensors[i]
            inds = SASQ.get_indices(tens)
            if inds[1] == inds[2] && t.constraints(inds[1]) == OccupiedOrbital
                deleteat!(t.tensors, i)
                deleteat!(t.sum_indices, findfirst(==(inds[1]), t.sum_indices))
                delete!(t.constraints, inds[1])
                push!(terms_with, t)
                pushed = true
            end
        end

        if !pushed
            push!(terms_without, t)
        end
    end

    SASQ.Expression(terms_with),
    SASQ.Expression(terms_without)
end

function W_oo_separated()
    W = W_oo()

    diag_terms = eltype(W.terms)[]
    other_terms = eltype(W.terms)[]

    for t in W.terms
        if length(t.deltas) == 1
            empty!(t.deltas)
            delete!(t.constraints, 1)
            delete!(t.constraints, 2)
            push!(diag_terms, t)
        else
            push!(other_terms, t)
        end
    end

    diag_ex = SASQ.Expression(diag_terms)
    other_ex = SASQ.Expression(other_terms)

    diag_ex_with_LJ_ii, diag_ex = separate_LJ_ii(diag_ex)
    other_ex_with_LJ_ii, other_ex = separate_LJ_ii(other_ex)

    diag_ex_with_LJ_ii, diag_ex, other_ex_with_LJ_ii, other_ex
end

function W_ov_separated()
    separate_LJ_ii(W_ov())
end

function W_vo_separated()
    separate_LJ_ii(W_vo())
end

function W_vv_separated()
    separate_LJ_ii(W_vv())
end
