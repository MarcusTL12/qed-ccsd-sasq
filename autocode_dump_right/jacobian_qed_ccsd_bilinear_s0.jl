let
    func = FortranFunction(("rho", String[]))
    ct_vo = ("ct_vo", true)
    d_ov = ("d_ov", true)
    cs_vo = ("cs_vo", true)
    γ₁ = ("wf%s0", false)
    cγ = ("cs", true)
    s_vo = ("wf%s1", false)
    update_code!(func, ein"ai,ia->", 2//1, [ct_vo, d_ov])
    update_code!(func, ein"ai,ia,->", 2//1, [cs_vo, d_ov, γ₁])
    update_code!(func, ein",ia,ai->", 2//1, [cγ, d_ov, s_vo])
    finalize_eT_function(func, "jacobian_qed_ccsd_bilinear_s0", "qed_ccsd")
end

