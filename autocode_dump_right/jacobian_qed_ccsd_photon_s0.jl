let
    func = FortranFunction(("rho", String[]))
    cγ = ("cs", true)
    ω = ("wf%qed%frequencies(wf%mode)", false)
    update_code!(func, ein",->", 1//1, [cγ, ω])
    finalize_eT_function(func, "jacobian_qed_ccsd_photon_s0", "qed_ccsd")
end

