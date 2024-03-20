let
    func = FortranFunction(("omega_vo", ["v", "o"]))
    d_vo = ("d_vo", true)
    γ₂ = ("wf%s0_1", false)
    d_vv = ("d_vv", true)
    s₂_vo = ("wf%s1_2", false)
    d_oo = ("d_oo", true)
    d_ov = ("d_ov", true)
    v₂_vovo = ("v₂_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    u_vovo = ("wf%u_aibj", false, [[1, 2, 3, 4], [3, 4, 1, 2]])
    update_code!(func, ein"ai,->ai", 2//1, [d_vo, γ₂])
    update_code!(func, ein"ab,bi->ai", 2//1, [d_vv, s₂_vo])
    update_code!(func, ein"ji,aj->ai", -2//1, [d_oo, s₂_vo])
    update_code!(func, ein"jj,ai->ai", 4//1, [d_oo, s₂_vo])
    update_code!(func, ein"jb,aibj->ai", 2//1, [d_ov, v₂_vovo])
    update_code!(func, ein"jb,aibj,->ai", 2//1, [d_ov, u_vovo, γ₂])
    finalize_eT_function(func, "omega_1_ai", "qed_ccsd_2")
end

