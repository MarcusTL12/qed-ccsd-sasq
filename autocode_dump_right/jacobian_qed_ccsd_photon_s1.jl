let
    func = FortranFunction(("rho_vo", ["v", "o"]))
    cs_vo = ("cs_vo", true)
    ω = ("wf%qed%frequencies(wf%mode)", false)
    update_code!(func, ein"ai,->ai", 1//1, [cs_vo, ω])
    finalize_eT_function(func, "jacobian_qed_ccsd_photon_s1", "qed_ccsd")
end

