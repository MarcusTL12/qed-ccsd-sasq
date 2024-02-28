let
    func = FortranFunction(("rho_vovo", ["v", "o", "v", "o"]))
    outperms = [[1, 2, 3, 4], [3, 4, 1, 2]]
    cs_vovo = ("cs_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    ω = ("wf%qed%frequencies(wf%mode)", false)
    update_code!(func, ein"aibj,->aibj", 1//1, [cs_vovo, ω], outperms)
    finalize_eT_function(func, "jacobian_qed_ccsd_photon_s2", "qed_ccsd")
end

