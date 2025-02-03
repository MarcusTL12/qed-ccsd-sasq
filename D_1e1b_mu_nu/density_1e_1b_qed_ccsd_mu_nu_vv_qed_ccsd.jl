let
    func = FortranFunction(("D_vv", ["v", "v"]))
    Ls_vo = ("Ls_vo", true)
    Rt_vo = ("Rt_vo", true)
    Lt_vo = ("Lt_vo", true)
    Rs_vo = ("Rs_vo", true)
    Rs = ("Rs", true)
    s_vo = ("s_vo", true)
    γ₁ = ("s0", true)
    Ls_vovo = ("Ls_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    Rt_vovo = ("Rt_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    Lt_vovo = ("Lt_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    Rs_vovo = ("Rs_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    s_vovo = ("s_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    t_vovo = ("t_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    update_code!(func, ein"ai,bi->ab", 1//1, [Ls_vo, Rt_vo])
    update_code!(func, ein"ai,bi->ab", 1//1, [Lt_vo, Rs_vo])
    update_code!(func, ein"ai,,bi->ab", 2//1, [Ls_vo, Rs, s_vo])
    update_code!(func, ein"ai,bi,->ab", 1//1, [Ls_vo, Rs_vo, γ₁])
    update_code!(func, ein"ai,bi,->ab", 1//1, [Lt_vo, Rt_vo, γ₁])
    update_code!(func, ein"aicj,bicj->ab", 1//1, [Ls_vovo, Rt_vovo])
    update_code!(func, ein"aicj,bicj->ab", 1//1, [Lt_vovo, Rs_vovo])
    update_code!(func, ein"aicj,,bicj->ab", 2//1, [Ls_vovo, Rs, s_vovo])
    update_code!(func, ein"aicj,bi,cj->ab", 1//1, [Ls_vovo, Rs_vo, s_vo])
    update_code!(func, ein"aicj,cj,bi->ab", 2//1, [Ls_vovo, Rs_vo, s_vo])
    update_code!(func, ein"aicj,bicj,->ab", 1//1, [Ls_vovo, Rs_vovo, γ₁])
    update_code!(func, ein"aicj,,bicj->ab", 1//1, [Lt_vovo, Rs, t_vovo])
    update_code!(func, ein"aicj,bi,cj->ab", 1//1, [Lt_vovo, Rt_vo, s_vo])
    update_code!(func, ein"aicj,cj,bi->ab", 1//1, [Lt_vovo, Rt_vo, s_vo])
    update_code!(func, ein"aicj,bicj,->ab", 1//1, [Lt_vovo, Rt_vovo, γ₁])
    update_code!(func, ein"aicj,,bicj,->ab", 1//1, [Ls_vovo, Rs, t_vovo, γ₁])
    update_code!(func, ein"aicj,cj,bi,->ab", 1//1, [Ls_vovo, Rt_vo, s_vo, γ₁])
    finalize_eT_function(func, "density_1e_1b_qed_ccsd_mu_nu_vv_qed_ccsd", "qed_ccsd")
end

