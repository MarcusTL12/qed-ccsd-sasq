
function make_code(ex, symbol, prefix, indices, outperms)
    open("$prefix.jl", "w") do io
        println(io, print_eT_function_generator(prefix, ex, symbol, indices, A_trans, "qed_ccsd",
            Dict([
                "cγ" => "cs",
                "bγ" => "bs",
                "γ₁" => "wf%s0",
                "t_vo" => "wf%t1",
                "s_vo" => "wf%s1",
                "u_vovo" => "wf%u_aibj",
                "ω" => "wf%qed%frequencies(wf%mode)"
            ]), ["γ₁", "t_vo", "s_vo", "u_vovo", "ω"], outperms))
    end
end

function flatten_perms(perms)
    [[i for (_, i) in p] for p in perms]
end

function jacobian_right_transformation(left_op, H, symmetry, symbol, name, H_name, indices)
    t1 = @time ∑(jacobian_element(left_op, ex_ketop(5, 6), H) * real_tensor("ct", 5, 6), 5:6)
    t1 = @time simplify(t1)
    @time begin
        t1 = look_for_tensor_replacements_smart(t1, make_exchange_transformer("t", "u"))
        t1 = look_for_tensor_replacements_smart(t1, make_exchange_transformer("s", "v"))
        t1 = look_for_tensor_replacements_smart(t1, make_exchange_transformer("g", "L"))
    end

    t1_s, t1_ss, t1_ns = @time desymmetrize(t1, make_permutation_mappings(symmetry))

    ex_s = t1_s
    ex_ss = t1_ss

    println("\nt1_s:\n")
    display(t1_s)
    println("\nt1_ss:\n")
    display(t1_ss)
    println("\nt1_ns:\n")
    display(t1_ns)

    t2 = @time (1 // 2 * ∑(jacobian_element(left_op, ex_ketop(5, 6, 7, 8), H) * psym_tensor("ct", 5:8...), 5:8))
    t2 = @time simplify(t2)
    @time begin
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("t", "u"))
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("s", "v"))
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("g", "L"))
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("ct", "cu"))
    end

    t2_s, t2_ss, t2_ns = @time desymmetrize(t2, make_permutation_mappings(symmetry))

    ex_s += t2_s
    ex_ss += t2_ss

    println("\nt2_s:\n")
    display(t2_s)
    println("\nt2_ss:\n")
    display(t2_ss)
    println("\nt2_ns:\n")
    display(t2_ns)

    γ = @time (jacobian_element(left_op, b', H) * real_tensor("cγ"))
    γ = @time simplify(γ)
    @time begin
        γ = look_for_tensor_replacements_smart(γ, make_exchange_transformer("t", "u"))
        γ = look_for_tensor_replacements_smart(γ, make_exchange_transformer("s", "v"))
        γ = look_for_tensor_replacements_smart(γ, make_exchange_transformer("g", "L"))
    end

    γ_s, γ_ss, γ_ns = @time desymmetrize(γ, make_permutation_mappings(symmetry))

    ex_s += γ_s
    ex_ss += γ_ss

    println("\nγ_s:\n")
    display(γ_s)
    println("\nγ_ss:\n")
    display(γ_ss)
    println("\nγ_ns:\n")
    display(γ_ns)

    s1 = @time (∑(jacobian_element(left_op, ex_ketop(5, 6) * b', H) * real_tensor("cs", 5, 6), 5:6))
    s1 = @time simplify(s1)
    @time begin
        s1 = look_for_tensor_replacements_smart(s1, make_exchange_transformer("t", "u"))
        s1 = look_for_tensor_replacements_smart(s1, make_exchange_transformer("s", "v"))
        s1 = look_for_tensor_replacements_smart(s1, make_exchange_transformer("g", "L"))
    end

    s1_s, s1_ss, s1_ns = @time desymmetrize(s1, make_permutation_mappings(symmetry))

    ex_s += s1_s
    ex_ss += s1_ss

    println("\ns1_s:\n")
    display(s1_s)
    println("\ns1_ss:\n")
    display(s1_ss)
    println("\ns1_ns:\n")
    display(s1_ns)

    s2 = @time (1 // 2 * ∑(jacobian_element(left_op, ex_ketop(5, 6, 7, 8) * b', H) * psym_tensor("cs", 5:8...), 5:8))
    s2 = @time simplify(s2)
    @time begin
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("t", "u"))
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("s", "v"))
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("g", "L"))
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("cs", "cv"))
    end

    s2_s, s2_ss, s2_ns = @time desymmetrize(s2, make_permutation_mappings(symmetry))

    ex_s += s2_s
    ex_ss += s2_ss

    println("\ns2_s:\n")
    display(s2_s)
    println("\ns2_ss:\n")
    display(s2_ss)
    println("\ns2_ns:\n")
    display(s2_ns)

    # outperms = if length(symmetry) > 1
    #     flatten_perms(make_permutation_mappings(symmetry))
    # else
    #     nothing
    # end

    # if iszero(ex_s)
    #     println("\nfinal ex_ss:")
    #     display((ex_ss, A_trans))

    #     if !iszero(ex_ss)
    #         make_code(ex_ss, symbol, "jacobian_qed_ccsd_$(H_name)_$(name)", indices, outperms)
    #     end
    # else
    #     println("\nfinal ex_s:")
    #     display((ex_s, A_trans))
    #     println("\nfinal ex_ss:")
    #     display((ex_ss, A_trans))

    #     if !iszero(ex_s)
    #         make_code(ex_s, symbol, "jacobian_qed_ccsd_$(H_name)_$(name)", indices, outperms)
    #     end
    #     if !iszero(ex_ss)
    #         make_code(ex_ss, symbol, "jacobian_qed_ccsd_$(H_name)_$(name)_sym", indices, outperms)
    #     end
    # end

    println()
end

function A_right_t1(H, H_name)
    println("ρt_ai =")
    jacobian_right_transformation(deex_braop(1, 2), H, [], "rho_vo", "t1", H_name, [1, 2])
end

function A_right_t2(H, H_name)
    println("ρt_aibj =")
    jacobian_right_transformation(deex_braop(1, 2, 3, 4), H, [(1, 2), (3, 4)], "rho_vovo", "t2", H_name, [1, 2, 3, 4])
end

function A_right_γ(H, H_name)
    println("ργ =")
    jacobian_right_transformation(b, H, [], "rho", "s0", H_name, [])
end

function A_right_s1(H, H_name)
    println("ρs_ai =")
    jacobian_right_transformation(deex_braop(1, 2) * b, H, [], "rho_vo", "s1", H_name, [1, 2])
end

function A_right_s2(H, H_name)
    println("ρs_aibj =")
    jacobian_right_transformation(deex_braop(1, 2, 3, 4) * b, H, [(1, 2), (3, 4)], "rho_vovo", "s2", H_name, [1, 2, 3, 4])
end
