using Serialization

h = ∑(real_tensor("h", 1, 2) * E(1, 2) * electron(1, 2), 1:2)
g = 1 // 2 * ∑(psym_tensor("g", 1, 2, 3, 4) * e(1, 2, 3, 4) * electron(1, 2, 3, 4), 1:4) |> simplify

H_ω = real_tensor("ω") * b' * b
H_Eb = ∑(real_tensor("d", 1, 2) * E(1, 2) * (b' + b) * electron(1, 2), 1:2)
H_b = -real_tensor("⟨d⟩") * (b' + b)

H = simplify(h + g + H_ω + H_Eb)

hF = ∑((
           real_tensor("F", 1, 2) +
           ∑((-2 // 1 * psym_tensor("g", 1, 2, 3, 3) +
              psym_tensor("g", 1, 3, 3, 2)) * occupied(3), [3])
       ) * E(1, 2) * electron(1, 2), 1:2)

HF = simplify(hF + g + H_ω + H_Eb + H_b)

He = simplify(h + g)
HFe = simplify(hF + g)

trans = translate(OccupiedOrbital => [2, 4], VirtualOrbital => [1, 3])

function omega(proj, op, n, symmetry=[])
    println()
    # expansion = @time bch(op, T, n)
    expansion = @time bch_smart(op, [T2, S1_1, S2_1, Γ1, S1_2, S2_2, Γ2], n)
    inner = simplify(proj * expansion)
    projected = @time hf_expectation_value(inner)
    Ω = @time simplify_heavy(projected)

    @time begin
        Ω = look_for_tensor_replacements_smart(Ω, make_exchange_transformer("t", "u"))
        Ω = look_for_tensor_replacements_smart(Ω, make_exchange_transformer("s₁", "v₁"))
        Ω = look_for_tensor_replacements_smart(Ω, make_exchange_transformer("s₂", "v₂"))
        Ω = look_for_tensor_replacements_smart(Ω, make_exchange_transformer("g", "L"))
    end

    if length(symmetry) != 0
        Ω_s, Ω_ss, _ = @time desymmetrize(Ω, make_permutation_mappings(symmetry))
        Ω = Ω_s + 1 // factorial(length(symmetry)) * Ω_ss
    end

    Ω
end

function make_omega(proj, Ω_name, symmetry=[]; HFe_n=2, H_ω_n=2, H_Eb_n=3)
    Ω_HFe = omega(proj, HFe, HFe_n, symmetry)
    println("\n\nH:  $Ω_name += \n", Ω_HFe)
    serialize("$(Ω_name)-HFe", Ω_HFe)

    Ω_H_ω = omega(proj, H_ω, H_ω_n, symmetry)
    println("\n\nω:  $Ω_name += \n", Ω_H_ω)
    serialize("$(Ω_name)-H_ω", Ω_H_ω)

    Ω_H_Eb = omega(proj, H_Eb, H_Eb_n, symmetry)
    println("\n\ned: $Ω_name += \n", Ω_H_Eb)
    serialize("$(Ω_name)-H_Eb", Ω_H_Eb)
end

function omega_t1()
    make_omega(deex_braop(1, 2), "Ω0_ai")
end

function omega_t2()
    make_omega(deex_braop(1, 2, 3, 4), "Ω0_aibj", [(1, 2), (3, 4)])
end

function omega_s0_1()
    make_omega(b, "Ω1")
end

function omega_s1_1()
    make_omega(b * deex_braop(1, 2), "Ω1_ai")
end

function omega_s2_1()
    make_omega(b * deex_braop(1, 2, 3, 4), "Ω1_aibj", [(1, 2), (3, 4)])
end

function omega_s0_2()
    make_omega(b^2, "Ω2")
end

function omega_s1_2()
    make_omega(b^2 * deex_braop(1, 2), "Ω2_ai")
end

function omega_s2_2()
    make_omega(b^2 * deex_braop(1, 2, 3, 4), "Ω2_aibj", [(1, 2), (3, 4)]; HFe_n=3)
end

function print_all_omegas_as_string_to_file()
    # omegas = ["Ω0_ai", "Ω0_aibj", "Ω1", "Ω1_ai", "Ω1_aibj", "Ω2", "Ω2_ai", "Ω2_aibj"]
    # Hs = ["HFe", "H_ω", "H_"]
    # for name in 
end
