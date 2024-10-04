let
    func = FortranFunction(("W_J_vo", ["J", "v", "o"]))
    LJ_tr = ("LJ_tr", true)
    Ls_vo = ("Ls_vo", true)
    Rs = ("Rs", true)
    LJ_oo = ("LJ_oo", true)
    LJ_ov = ("LJ_ov", true)
    Rs_vo = ("Rs_vo", true)
    LJ_vo = ("LJ_vo", true)
    Ls_vovo = ("Ls_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    LJ_vv = ("LJ_vv", true)
    Rs_vovo = ("Rs_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    Rv_vovo = ("Rv_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    t_vovo = ("t_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    u_vovo = ("u_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    Rt_vo = ("Rt_vo", true)
    s_vo = ("s_vo", true)
    update_code!(func, ein"h,ai,->hai", 2//1, [LJ_tr, Ls_vo, Rs])
    update_code!(func, ein"hij,aj,->hai", -1//1, [LJ_oo, Ls_vo, Rs])
    update_code!(func, ein"hib,aj,bj->hai", -1//1, [LJ_ov, Ls_vo, Rs_vo])
    update_code!(func, ein"hjb,ai,bj->hai", 2//1, [LJ_ov, Ls_vo, Rs_vo])
    update_code!(func, ein"hbj,aibj,->hai", 1//1, [LJ_vo, Ls_vovo, Rs])
    update_code!(func, ein"h,aibj,bj->hai", 2//1, [LJ_tr, Ls_vovo, Rs_vo])
    update_code!(func, ein"hij,ajbk,bk->hai", -1//1, [LJ_oo, Ls_vovo, Rs_vo])
    update_code!(func, ein"hbc,aibj,cj->hai", 1//1, [LJ_vv, Ls_vovo, Rs_vo])
    update_code!(func, ein"hjk,aibk,bj->hai", -1//1, [LJ_oo, Ls_vovo, Rs_vo])
    update_code!(func, ein"hib,ajck,bjck->hai", -1//1, [LJ_ov, Ls_vovo, Rs_vovo])
    update_code!(func, ein"hjb,aick,bjck->hai", 1//1, [LJ_ov, Ls_vovo, Rv_vovo])
    update_code!(func, ein"hib,ajck,,bjck->hai", -1//1, [LJ_ov, Ls_vovo, Rs, t_vovo])
    update_code!(func, ein"hjb,aick,,bjck->hai", 1//1, [LJ_ov, Ls_vovo, Rs, u_vovo])
    update_code!(func, ein"hib,ajck,ck,bj->hai", -1//1, [LJ_ov, Ls_vovo, Rt_vo, s_vo])
    update_code!(func, ein"hjb,aick,bk,cj->hai", -1//1, [LJ_ov, Ls_vovo, Rt_vo, s_vo])
    update_code!(func, ein"hjb,aick,cj,bk->hai", -1//1, [LJ_ov, Ls_vovo, Rt_vo, s_vo])
    update_code!(func, ein"hjb,aick,ck,bj->hai", 2//1, [LJ_ov, Ls_vovo, Rt_vo, s_vo])
    finalize_eT_function(func, "W_J_vo_terms", "qed_ccsd")
end

