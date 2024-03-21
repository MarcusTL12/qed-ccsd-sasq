
function jacobian_left_transformation(right_op, H, symmetry, symbol, name, H_name, indices)
    t1 = simplify_heavy(∑(jacobian_element(deex_braop(5, 6), right_op, H) * real_tensor("bt", 5, 6), 5:6))
    @time begin
        t1 = look_for_tensor_replacements_smart(t1, make_exchange_transformer("t", "u"))
        t1 = look_for_tensor_replacements_smart(t1, make_exchange_transformer("s₁", "v₁"))
        t1 = look_for_tensor_replacements_smart(t1, make_exchange_transformer("s₂", "v₂"))
        t1 = look_for_tensor_replacements_smart(t1, make_exchange_transformer("g", "L"))
    end

    t1_s, t1_ss, t1_ns = desymmetrize(t1, make_permutation_mappings(symmetry))

    ex_s = t1_s
    ex_ss = t1_ss

    println("\nt1_s:\n")
    display((t1_s, A_trans))
    println("\nt1_ss:\n")
    display((t1_ss, A_trans))

    t2 = simplify_heavy(1 // 2 * ∑(jacobian_element(deex_braop(5, 6, 7, 8), right_op, H) * psym_tensor("bt", 5:8...), 5:8))
    @time begin
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("t", "u"))
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("s₁", "v₁"))
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("s₂", "v₂"))
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("g", "L"))
        t2 = look_for_tensor_replacements_smart(t2, make_exchange_transformer("bt", "bu"))
    end

    t2_s, t2_ss, t2_ns = desymmetrize(t2, make_permutation_mappings(symmetry))

    ex_s += t2_s
    ex_ss += t2_ss

    println("\nt2_s:\n")
    display((t2_s, A_trans))
    println("\nt2_ss:\n")
    display((t2_ss, A_trans))

    γ = simplify_heavy(jacobian_element(b, right_op, H) * real_tensor("bγ"))
    @time begin
        γ = look_for_tensor_replacements_smart(γ, make_exchange_transformer("t", "u"))
        γ = look_for_tensor_replacements_smart(γ, make_exchange_transformer("s₁", "v₁"))
        γ = look_for_tensor_replacements_smart(γ, make_exchange_transformer("s₂", "v₂"))
        γ = look_for_tensor_replacements_smart(γ, make_exchange_transformer("g", "L"))
    end

    γ_s, γ_ss, γ_ns = desymmetrize(γ, make_permutation_mappings(symmetry))

    ex_s += γ_s
    ex_ss += γ_ss

    println("\nγ_s:\n")
    display((γ_s, A_trans))
    println("\nγ_ss:\n")
    display((γ_ss, A_trans))

    s1 = simplify_heavy(∑(jacobian_element(deex_braop(5, 6) * b, right_op, H) * real_tensor("bs", 5, 6), 5:6))
    @time begin
        s1 = look_for_tensor_replacements_smart(s1, make_exchange_transformer("t", "u"))
        s1 = look_for_tensor_replacements_smart(s1, make_exchange_transformer("s₁", "v₁"))
        s1 = look_for_tensor_replacements_smart(s1, make_exchange_transformer("s₂", "v₂"))
        s1 = look_for_tensor_replacements_smart(s1, make_exchange_transformer("g", "L"))
    end

    s1_s, s1_ss, s1_ns = desymmetrize(s1, make_permutation_mappings(symmetry))

    ex_s += s1_s
    ex_ss += s1_ss

    println("\ns1_s:\n")
    display((s1_s, A_trans))
    println("\ns1_ss:\n")
    display((s1_ss, A_trans))

    s2 = simplify_heavy(1 // 2 * ∑(jacobian_element(deex_braop(5, 6, 7, 8) * b, right_op, H) * psym_tensor("bs", 5:8...), 5:8))
    @time begin
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("t", "u"))
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("s₁", "v₁"))
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("s₂", "v₂"))
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("g", "L"))
        s2 = look_for_tensor_replacements_smart(s2, make_exchange_transformer("bs", "bv"))
    end

    s2_s, s2_ss, s2_ns = desymmetrize(s2, make_permutation_mappings(symmetry))

    ex_s += s2_s
    ex_ss += s2_ss

    println("\ns2_s:\n")
    display((s2_s, A_trans))
    println("\ns2_ss:\n")
    display((s2_ss, A_trans))

    # γ2 = simplify_heavy(jacobian_element(b^2, right_op, H) * real_tensor("bγ2"))
    # @time begin
    #     γ2 = look_for_tensor_replacements_smart(γ2, make_exchange_transformer("t", "u"))
    #     γ2 = look_for_tensor_replacements_smart(γ2, make_exchange_transformer("s₁", "v₁"))
    #     γ2 = look_for_tensor_replacements_smart(γ2, make_exchange_transformer("s₂", "v₂"))
    #     γ2 = look_for_tensor_replacements_smart(γ2, make_exchange_transformer("g", "L"))
    # end

    # γ2_s, γ2_ss, γ2_ns = desymmetrize(γ2, make_permutation_mappings(symmetry))

    # ex_s += γ2_s
    # ex_ss += γ2_ss

    # println("\nγ2_s:\n")
    # display((γ2_s, A_trans))
    # println("\nγ2_ss:\n")
    # display((γ2_ss, A_trans))

    # s1_2 = simplify_heavy(∑(jacobian_element(deex_braop(5, 6) * b^2, right_op, H) * real_tensor("bs2", 5, 6), 5:6))
    # @time begin
    #     s1_2 = look_for_tensor_replacements_smart(s1_2, make_exchange_transformer("t", "u"))
    #     s1_2 = look_for_tensor_replacements_smart(s1_2, make_exchange_transformer("s₁", "v₁"))
    #     s1_2 = look_for_tensor_replacements_smart(s1_2, make_exchange_transformer("s₂", "v₂"))
    #     s1_2 = look_for_tensor_replacements_smart(s1_2, make_exchange_transformer("g", "L"))
    # end

    # s1_2_s, s1_2_ss, s1_2_ns = desymmetrize(s1, make_permutation_mappings(symmetry))

    # ex_s += s1_2_s
    # ex_ss += s1_2_ss

    # println("\ns1_2_s:\n")
    # display((s1_2_s, A_trans))
    # println("\ns1_2_ss:\n")
    # display((s1_2_ss, A_trans))

    # s2_2 = simplify_heavy(1 // 2 * ∑(jacobian_element(deex_braop(5, 6, 7, 8) * b^2, right_op, H) * psym_tensor("bs2", 5:8...), 5:8))
    # @time begin
    #     s2_2 = look_for_tensor_replacements_smart(s2_2, make_exchange_transformer("t", "u"))
    #     s2_2 = look_for_tensor_replacements_smart(s2_2, make_exchange_transformer("s₁", "v₁"))
    #     s2_2 = look_for_tensor_replacements_smart(s2_2, make_exchange_transformer("s₂", "v₂"))
    #     s2_2 = look_for_tensor_replacements_smart(s2_2, make_exchange_transformer("g", "L"))
    #     s2_2 = look_for_tensor_replacements_smart(s2_2, make_exchange_transformer("bs2", "bv2"))
    # end

    # s2_2_s, s2_2_ss, s2_2_ns = desymmetrize(s2_2, make_permutation_mappings(symmetry))

    # ex_s += s2_2_s
    # ex_ss += s2_2_ss

    # println("\ns2_2_s:\n")
    # display((s2_2_s, A_trans))
    # println("\ns2_2_ss:\n")
    # display((s2_2_ss, A_trans))

    ex_s += ex_ss * 1 // factorial(length(symmetry))

    if !iszero(ex_s)
        serialize("A_left_$(name)_$(H_name)", ex_s)
    end

    # outperms = if length(symmetry) > 1
    #     flatten_perms(make_permutation_mappings(symmetry))
    # else
    #     nothing
    # end

    # if iszero(ex_s)
    #     println("\nfinal ex_ss:")
    #     display((ex_ss, A_trans))

    #     if !iszero(ex_ss)
    #         make_code(ex_ss, symbol, "jacobian_transpose_qed_ccsd_$(H_name)_$(name)", indices, outperms)
    #     end
    # else
    #     println("\nfinal ex_s:")
    #     display((ex_s, A_trans))
    #     println("\nfinal ex_ss:")
    #     display((ex_ss, A_trans))

    #     if !iszero(ex_s)
    #         make_code(ex_s, symbol, "jacobian_transpose_qed_ccsd_$(H_name)_$(name)", indices, outperms)
    #     end
    #     if !iszero(ex_ss)
    #         make_code(ex_ss, symbol, "jacobian_transpose_qed_ccsd_$(H_name)_$(name)_sym", indices, outperms)
    #     end
    # end

    println()
end

function A_left_t1(H, H_name)
    println("σt_ai =")
    jacobian_left_transformation(ex_ketop(1, 2), H, [], "sigma_vo", "t1", H_name, [1, 2])
end

function A_left_t2(H, H_name)
    println("σt_aibj =")
    jacobian_left_transformation(ex_ketop(1, 2, 3, 4), H, [(1, 2), (3, 4)], "sigma_vovo", "t2", H_name, [1, 2, 3, 4])
end

function A_left_γ1(H, H_name)
    println("σγ =")
    jacobian_left_transformation(b', H, [], "sigma", "s0", H_name, [])
end

function A_left_s1_1(H, H_name)
    println("σs_ai =")
    jacobian_left_transformation(ex_ketop(1, 2) * b', H, [], "sigma_vo", "s1", H_name, [1, 2])
end

function A_left_s2_1(H, H_name)
    println("σs_aibj =")
    jacobian_left_transformation(ex_ketop(1, 2, 3, 4) * b', H, [(1, 2), (3, 4)], "sigma_vovo", "s2", H_name, [1, 2, 3, 4])
end

function A_left_γ2(H, H_name)
    println("σγ =")
    jacobian_left_transformation(b'^2, H, [], "sigma", "s0_2", H_name, [])
end

function A_left_s1_2(H, H_name)
    println("σs_ai =")
    jacobian_left_transformation(ex_ketop(1, 2) * b'^2, H, [], "sigma_vo", "s1_2", H_name, [1, 2])
end

function A_left_s2_2(H, H_name)
    println("σs_aibj =")
    jacobian_left_transformation(ex_ketop(1, 2, 3, 4) * b'^2, H, [(1, 2), (3, 4)], "sigma_vovo", "s2_2", H_name, [1, 2, 3, 4])
end

function A_all_left(H, H_name)
    A_left_t1(H, H_name)
    A_left_t2(H, H_name)
    A_left_γ1(H, H_name)
    A_left_s1_1(H, H_name)
    A_left_s2_1(H, H_name)
    # A_left_γ2(H, H_name)
    # A_left_s1_2(H, H_name)
    # A_left_s2_2(H, H_name)
end

function make_A_left_differences()
    for name in readdir("jacobian_left_saves/full_2_serial")
        if isfile("jacobian_left_saves/full_1_serial/$name")
            ex1 = deserialize("jacobian_left_saves/full_1_serial/$name")
            ex2 = deserialize("jacobian_left_saves/full_2_serial/$name")
            Δ = ex2 - ex1
            serialize("jacobian_left_saves/reduced_2_serial/$name", Δ)
        else
            ex2 = deserialize("jacobian_left_saves/full_2_serial/$name")
            serialize("jacobian_left_saves/reduced_2_serial/$name", ex2)
        end
    end
end

function generate_code_A_left(dir, name, eT_name, eT_symbol, indices, outperms=nothing)
    Hs = ["electronic", "photon", "bilinear"]

    ex = SASQ.Expression(0)

    for H in Hs
        filename = "$dir/$(name)_$H"
        if isfile(filename)
            ex += deserialize(filename)
        end
    end

    open("$(name).jl", "w") do io
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

function generate_all_code_A_left()
    generate_code_A_left("jacobian_left_saves/reduced_2_serial/", "A_left_t1", "jacobian_transpose_t1", "sigma_vo", [1, 2])
    generate_code_A_left("jacobian_left_saves/reduced_2_serial/", "A_left_t2", "jacobian_transpose_t2", "sigma_vovo", [1, 2, 3, 4], [[1, 2, 3, 4], [3, 4, 1, 2]])

    generate_code_A_left("jacobian_left_saves/reduced_2_serial/", "A_left_s0", "jacobian_transpose_s0", "sigma", [])
    generate_code_A_left("jacobian_left_saves/reduced_2_serial/", "A_left_s1", "jacobian_transpose_s1", "sigma_vo", [1, 2])
    generate_code_A_left("jacobian_left_saves/reduced_2_serial/", "A_left_s2", "jacobian_transpose_s2", "sigma_vovo", [1, 2, 3, 4], [[1, 2, 3, 4], [3, 4, 1, 2]])

    generate_code_A_left("jacobian_left_saves/reduced_2_serial/", "A_left_s0_2", "jacobian_transpose_s0_2", "sigma", [])
    generate_code_A_left("jacobian_left_saves/reduced_2_serial/", "A_left_s1_2", "jacobian_transpose_s1_2", "sigma_vo", [1, 2])
    generate_code_A_left("jacobian_left_saves/reduced_2_serial/", "A_left_s2_2", "jacobian_transpose_s2_2", "sigma_vovo", [1, 2, 3, 4], [[1, 2, 3, 4], [3, 4, 1, 2]])
end
