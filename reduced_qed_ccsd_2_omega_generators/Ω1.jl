let
    func = FortranFunction(("omega", String[]))
    d_oo = ("d_oo", true)
    γ₂ = ("wf%s0_1", false)
    d_ov = ("d_ov", true)
    s₂_vo = ("wf%s1_2", false)
    update_code!(func, ein"ii,->", 4//1, [d_oo, γ₂])
    update_code!(func, ein"ia,ai->", 4//1, [d_ov, s₂_vo])
    finalize_eT_function(func, "omega_1", "qed_ccsd_2")
end

