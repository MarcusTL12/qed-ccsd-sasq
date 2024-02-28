
function energy_expression()
    E_hf = hf_expectation_value(h + g) |> simplify_heavy
    @show E_hf
    println()

    E_h = hf_expectation_value(bch(h, T, 2) - h) |> simplify_heavy
    E_g = hf_expectation_value(bch(g, T, 2) - g) |> simplify_heavy
    E_ω = hf_expectation_value(bch(H_ω, T, 2)) |> simplify_heavy
    E_Eb = hf_expectation_value(bch(H_Eb, T, 3)) |> simplify_heavy
    E_b = hf_expectation_value(bch(H_b, T, 2)) |> simplify_heavy

    @show E_h
    println()
    @show E_g
    println()
    @show E_ω
    println()
    @show E_Eb
    println()
    @show E_b
end
