let
    func = FortranFunction(("rho_vovo", ["v", "o", "v", "o"]))
    outperms = [[1, 2, 3, 4], [3, 4, 1, 2]]
    cs_vovo = ("cs_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    d_oo = ("d_oo", true)
    ct_vo = ("ct_vo", true)
    d_ov = ("d_ov", true)
    s_vovo = ("s_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    update_code!(func, ein"aibj,kk->aibj", 2//1, [cs_vovo, d_oo], outperms)
    update_code!(func, ein"ck,kc,aibj->aibj", 2//1, [ct_vo, d_ov, s_vovo], outperms)
    finalize_eT_function(func, "jacobian_qed_ccsd_bilinear_t2_sym", "qed_ccsd")
end

