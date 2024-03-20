let
    func = FortranFunction(("omega_vovo", ["v", "o", "v", "o"]))
    outperms = [[1, 2, 3, 4], [3, 4, 1, 2]]
    d_vo = ("d_vo", true)
    s₂_vo = ("wf%s1_2", false)
    d_vv = ("d_vv", true)
    s₂_vovo = ("s₂_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    d_oo = ("d_oo", true)
    t_vovo = ("t_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    γ₂ = ("wf%s0_1", false)
    d_ov = ("d_ov", true)
    u_vovo = ("wf%u_aibj", false, [[1, 2, 3, 4], [3, 4, 1, 2]])
    update_code!(func, ein"ai,bj->aibj", 2//1, [d_vo, s₂_vo], outperms)
    update_code!(func, ein"ac,bjci->aibj", 2//1, [d_vv, s₂_vovo], outperms)
    update_code!(func, ein"ki,akbj->aibj", -2//1, [d_oo, s₂_vovo], outperms)
    update_code!(func, ein"kk,aibj->aibj", 2//1, [d_oo, s₂_vovo], outperms)
    update_code!(func, ein"ac,bjci,->aibj", 2//1, [d_vv, t_vovo, γ₂], outperms)
    update_code!(func, ein"ki,akbj,->aibj", -2//1, [d_oo, t_vovo, γ₂], outperms)
    update_code!(func, ein"kc,ak,bjci->aibj", -2//1, [d_ov, s₂_vo, t_vovo], outperms)
    update_code!(func, ein"kc,ci,akbj->aibj", -2//1, [d_ov, s₂_vo, t_vovo], outperms)
    update_code!(func, ein"kc,ai,bjck->aibj", 2//1, [d_ov, s₂_vo, u_vovo], outperms)
    finalize_eT_function(func, "omega_1_aibj", "qed_ccsd_2")
end

