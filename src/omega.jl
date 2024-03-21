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
    # expansion = @time bch(op, T2, n)
    # expansion = @time bch_smart(op, [T2, S1_1, S2_1, Γ1], n)
    # expansion = @time bch_smart(op, [T2, S1_1, S2_1, Γ1, S1_2, S2_2, Γ2], n)
    # inner = simplify(proj * expansion)
    # projected = @time hf_expectation_value(inner)
    # Ω = @time simplify_heavy(projected)

    x = @time act_eT_on_bra(proj, -T)
    x = @time act_on_bra(x * op) |> simplify
    x = @time act_eT_on_bra(x, T; max_ops=0)
    Ω = @time simplify_heavy(x)

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

function print_all_omegas_as_string_to_file(topname="full_qed_ccsd_2_omega")
    for name in readdir("$(topname)_serial/")
        ex = deserialize("$(topname)_serial/$name")
        open("$(topname)_dump/$(name).txt", "w") do io
            println(io, ex)
        end
    end
end

function make_omega_differences()
    for name in readdir("full_qed_ccsd_1_omega_serial/")
        ex1 = deserialize("full_qed_ccsd_1_omega_serial/$name")
        ex2 = deserialize("full_qed_ccsd_2_omega_serial/$name")
        Δ = ex2 - ex1
        serialize("reduced_qed_ccsd_2_omega_serial/$name", Δ)
    end
end

function filter_zeros(dir)
    for name in readdir(dir)
        ex = deserialize("$dir/$name")
        if iszero(ex)
            rm("$dir/$name")
        end
    end
end

function generate_code_omega(dir, omega_name, eT_name, eT_symbol, indices, outperms=nothing)
    Hs = ["HFe", "H_ω", "H_Eb"]

    ex = SASQ.Expression(0)

    for H in Hs
        filename = "$dir/$omega_name-$H"
        if isfile(filename)
            ex += deserialize(filename)
        end
    end

    open("$(omega_name).jl", "w") do io
        println(io, print_eT_function_generator(eT_name, ex, eT_symbol, indices, A_trans, "qed_ccsd_2",
            Dict([
                "γ₁" => "wf%s0",
                "γ₂" => "wf%s0_1",
                "t_vo" => "wf%t1",
                "s₁_vo" => "wf%s1",
                "s₂_vo" => "wf%s1_2",
                "u_vovo" => "wf%u_aibj",
                "ω" => "wf%qed%frequencies(wf%mode)"
            ]), ["γ₁", "γ₂", "t_vo", "s₁_vo", "s₂_vo", "u_vovo", "ω"], outperms))
    end
end

function generate_all_code_omega()
    generate_code_omega("reduced_qed_ccsd_2_omega_serial/", "Ω1", "omega_1", "omega", [])
    generate_code_omega("reduced_qed_ccsd_2_omega_serial/", "Ω1_ai", "omega_1_ai", "omega_vo", [1, 2])
    generate_code_omega("reduced_qed_ccsd_2_omega_serial/", "Ω1_aibj", "omega_1_aibj", "omega_vovo", [1, 2, 3, 4], [[1, 2, 3, 4], [3, 4, 1, 2]])

    generate_code_omega("reduced_qed_ccsd_2_omega_serial/", "Ω2", "omega_2", "omega", [])
    generate_code_omega("reduced_qed_ccsd_2_omega_serial/", "Ω2_ai", "omega_2_ai", "omega_vo", [1, 2])
    generate_code_omega("reduced_qed_ccsd_2_omega_serial/", "Ω2_aibj", "omega_2_aibj", "omega_vovo", [1, 2, 3, 4], [[1, 2, 3, 4], [3, 4, 1, 2]])
end
