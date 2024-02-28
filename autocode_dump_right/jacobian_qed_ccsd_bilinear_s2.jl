let
    func = FortranFunction(("rho_vovo", ["v", "o", "v", "o"]))
    outperms = [[1, 2, 3, 4], [3, 4, 1, 2]]
    ct_vovo = ("ct_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    d_oo = ("d_oo", true)
    d_vv = ("d_vv", true)
    cs_vo = ("cs_vo", true)
    s_vo = ("wf%s1", false)
    cs_vovo = ("cs_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    γ₁ = ("wf%s0", false)
    cγ = ("cs", true)
    s_vovo = ("s_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    d_ov = ("d_ov", true)
    v_vovo = ("v_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    ct_vo = ("ct_vo", true)
    t_vovo = ("t_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    cv_vovo = ("cv_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    update_code!(func, ein"aibk,kj->aibj", -1//1, [ct_vovo, d_oo], outperms)
    update_code!(func, ein"aicj,bc->aibj", 1//1, [ct_vovo, d_vv], outperms)
    update_code!(func, ein"ai,bc,cj->aibj", 1//1, [cs_vo, d_vv, s_vo], outperms)
    update_code!(func, ein"ai,kj,bk->aibj", -1//1, [cs_vo, d_oo, s_vo], outperms)
    update_code!(func, ein"ak,ki,bj->aibj", -1//1, [cs_vo, d_oo, s_vo], outperms)
    update_code!(func, ein"ci,ac,bj->aibj", 1//1, [cs_vo, d_vv, s_vo], outperms)
    update_code!(func, ein"aibk,kj,->aibj", -1//1, [cs_vovo, d_oo, γ₁], outperms)
    update_code!(func, ein"aicj,bc,->aibj", 1//1, [cs_vovo, d_vv, γ₁], outperms)
    update_code!(func, ein",ac,bjci->aibj", 1//1, [cγ, d_vv, s_vovo], outperms)
    update_code!(func, ein",ki,akbj->aibj", -1//1, [cγ, d_oo, s_vovo], outperms)
    update_code!(func, ein"aibk,kc,cj->aibj", -2//1, [cs_vovo, d_ov, s_vo], outperms)
    update_code!(func, ein"aicj,kc,bk->aibj", -2//1, [cs_vovo, d_ov, s_vo], outperms)
    update_code!(func, ein"ak,kc,bjci->aibj", -2//1, [cs_vo, d_ov, s_vovo], outperms)
    update_code!(func, ein"ci,kc,akbj->aibj", -2//1, [cs_vo, d_ov, s_vovo], outperms)
    update_code!(func, ein"ai,kc,bjck->aibj", 1//1, [cs_vo, d_ov, v_vovo], outperms)
    update_code!(func, ein"ak,kc,bjci->aibj", -1//1, [ct_vo, d_ov, t_vovo], outperms)
    update_code!(func, ein"ci,kc,akbj->aibj", -1//1, [ct_vo, d_ov, t_vovo], outperms)
    update_code!(func, ein"aick,kc,bj->aibj", 1//1, [cv_vovo, d_ov, s_vo], outperms)
    update_code!(func, ein"ak,kc,bjci,->aibj", -1//1, [cs_vo, d_ov, t_vovo, γ₁], outperms)
    update_code!(func, ein"ci,kc,akbj,->aibj", -1//1, [cs_vo, d_ov, t_vovo, γ₁], outperms)
    update_code!(func, ein"ak,kc,bj,ci->aibj", -1//1, [ct_vo, d_ov, s_vo, s_vo], outperms)
    update_code!(func, ein"ci,kc,ak,bj->aibj", -1//1, [ct_vo, d_ov, s_vo, s_vo], outperms)
    update_code!(func, ein"aibk,kc,cj,->aibj", -1//1, [ct_vovo, d_ov, s_vo, γ₁], outperms)
    update_code!(func, ein"aicj,kc,bk,->aibj", -1//1, [ct_vovo, d_ov, s_vo, γ₁], outperms)
    update_code!(func, ein"ak,kc,bjci,->aibj", -1//1, [ct_vo, d_ov, s_vovo, γ₁], outperms)
    update_code!(func, ein"ci,kc,akbj,->aibj", -1//1, [ct_vo, d_ov, s_vovo, γ₁], outperms)
    update_code!(func, ein",kc,ak,bjci->aibj", -1//1, [cγ, d_ov, s_vo, t_vovo], outperms)
    update_code!(func, ein",kc,ci,akbj->aibj", -1//1, [cγ, d_ov, s_vo, t_vovo], outperms)
    finalize_eT_function(func, "jacobian_qed_ccsd_bilinear_s2", "qed_ccsd")
end

