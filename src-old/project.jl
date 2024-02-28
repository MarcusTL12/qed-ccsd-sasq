
function excitation_order(t)
    photon_count = 0
    electron_count = 0

    for o in t.operators
        if o isa SASQ.BosonOperator
            photon_count += 1
        elseif o isa SASQ.SingletExcitationOperator
            electron_count += 1
        end
    end

    (photon_count, electron_count)
end

function expr(t)
    if isempty(t)
        zero(SASQ.Expression{Int})
    else
        SASQ.Expression(t)
    end
end

function remove_ops(t)
    t = copy(t)
    empty!(t.operators)
    t
end

function project_10(ex)
    real_tensor("γᴸ") *
    expr(
        [remove_ops(x) for x in ex.terms if excitation_order(x) == (1, 0)]
    )
end

function project_1_term(t)
    t = copy(t)
    Eai = t.operators[
        findfirst(x -> x isa SASQ.SingletExcitationOperator, t.operators)
    ]

    a = Eai.p
    i = Eai.q

    d1 = SASQ.KroneckerDelta(5, a)
    d2 = SASQ.KroneckerDelta(6, i)

    if d1 == 0 || d2 == 0
        return zero(SASQ.Term)
    end

    d1 != 1 && push!(t.deltas, d1)
    d2 != 1 && push!(t.deltas, d2)

    empty!(t.operators)

    t
end

function project_11(ex)
    x = sl(5, 6) * expr(
        [x for x in ex.terms if excitation_order(x) == (1, 1)]
    )

    if iszero(x)
        return x
    end

    simplify(∑(expr([project_1_term(t) for t in x.terms]), 5:6))
end

function project_2_term(t)
    t = copy(t)
    Eai = t.operators[
        findfirst(x -> x isa SASQ.SingletExcitationOperator, t.operators)
    ]

    Ebj = t.operators[
        findlast(x -> x isa SASQ.SingletExcitationOperator, t.operators)
    ]

    a = Eai.p
    i = Eai.q
    b = Ebj.p
    j = Ebj.q

    d1 = SASQ.KroneckerDelta(5, a)
    d2 = SASQ.KroneckerDelta(6, i)

    d3 = SASQ.KroneckerDelta(7, b)
    d4 = SASQ.KroneckerDelta(8, j)

    if d1 == 0 || d2 == 0 || d3 == 0 || d4 == 0
        return zero(SASQ.Term)
    end

    d1 != 1 && push!(t.deltas, d1)
    d2 != 1 && push!(t.deltas, d2)
    d3 != 1 && push!(t.deltas, d3)
    d4 != 1 && push!(t.deltas, d4)

    push!(t.tensors, SASQ.PermuteTensor(collect(5:8)))

    empty!(t.operators)

    t
end

function project_12(ex)
    x = st(5, 6, 7, 8) * expr(
        [x for x in ex.terms if excitation_order(x) == (1, 2)]
    )

    if iszero(x)
        return x
    end

    simplify(∑(expr([project_2_term(t) for t in x.terms]), 5:8))
end

function project_00(ex)
    expr(
        [remove_ops(x) for x in ex.terms if excitation_order(x) == (0, 0)]
    )
end

function project_01(ex)
    x = tl(5, 6) * expr(
        [x for x in ex.terms if excitation_order(x) == (0, 1)]
    )

    if iszero(x)
        return x
    end

    simplify(∑(expr([project_1_term(t) for t in x.terms]), 5:6))
end

function project_02(ex)
    x = tt(5, 6, 7, 8) * expr(
        [x for x in ex.terms if excitation_order(x) == (0, 2)]
    )

    if iszero(x)
        return x
    end

    simplify_heavy(∑(expr([project_2_term(t) for t in x.terms]), 5:8))
end

function get_densities_P(op)
    exop = @time (bch(op, T, 2) |> act_on_ket |> simplify)

    D00 = @time project_00(exop)
    D01 = @time project_01(exop)
    D02 = @time project_02(exop)

    D10 = @time project_10(exop)
    D11 = @time project_11(exop)
    D12 = @time project_12(exop)

    @show D00 D01 D02
    println()
    @show D10 D11 D12

    nothing
end

function photon_density_P()
    get_densities_P(b'b)
end

# Calculate ⟨Λ| b† + b |CC⟩
function photon_density_2_P()
    get_densities_P(b' + b)
end

function one_electron_density_P()
    get_densities_P(E(1, 2))
end

function two_electron_density_P()
    get_densities_P(e(1, 2, 3, 4))
end

function one_electron_one_photon_density_P()
    get_densities_P(E(1, 2) * (b' + b))
end
