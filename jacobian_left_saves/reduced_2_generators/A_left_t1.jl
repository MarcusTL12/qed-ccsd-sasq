let
    func = FortranFunction(("sigma_vo", ["v", "o"]))
    bγ = ("bγ", true)
    d_ov = ("d_ov", true)
    γ₂ = ("wf%s0_1", false)
    bs_vo = ("bs_vo", true)
    d_oo = ("d_oo", true)
    d_vv = ("d_vv", true)
    F_ov = ("F_ov", true)
    s₁_vo = ("wf%s1", false)
    L_ovov = ("L_ovov", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    bγ2 = ("bγ2", true)
    s₂_vo = ("wf%s1_2", false)
    γ₁ = ("wf%s0", false)
    L_ovvv = ("L_ovvv", true)
    L_ovoo = ("L_ovoo", true)
    L_ooov = ("L_ooov", true)
    L_vvov = ("L_vvov", true)
    bs_vovo = ("bs_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    bs2_vovo = ("bs2_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    s₂_vovo = ("s₂_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    s₁_vovo = ("s₁_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    g_oovv = ("g_oovv", true)
    g_oooo = ("g_oooo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    g_ovvo = ("g_ovvo", true)
    g_vvvv = ("g_vvvv", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    g_vvoo = ("g_vvoo", true)
    g_voov = ("g_voov", true)
    t_vovo = ("t_vovo", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    g_ooov = ("g_ooov", true)
    g_ovvv = ("g_ovvv", true)
    g_ovoo = ("g_ovoo", true)
    g_vvov = ("g_vvov", true)
    g_ovov = ("g_ovov", true, [[1, 2, 3, 4], [3, 4, 1, 2]])
    u_vovo = ("wf%u_aibj", false, [[1, 2, 3, 4], [3, 4, 1, 2]])
    update_code!(func, ein",ia,->ai", 4//1, [bγ, d_ov, γ₂])
    update_code!(func, ein"aj,ij->ai", -1//1, [bs_vo, d_oo])
    update_code!(func, ein"bi,ba->ai", 1//1, [bs_vo, d_vv])
    update_code!(func, ein"aj,ij,->ai", -4//1, [bs_vo, d_oo, γ₂])
    update_code!(func, ein"bi,ba,->ai", 4//1, [bs_vo, d_vv, γ₂])
    update_code!(func, ein"ib,aj,bj->ai", -1//1, [F_ov, bs_vo, s₁_vo])
    update_code!(func, ein"ja,bi,bj->ai", -1//1, [F_ov, bs_vo, s₁_vo])
    update_code!(func, ein"iajb,,bj->ai", 4//1, [L_ovov, bγ2, s₂_vo])
    update_code!(func, ein"aj,ib,bj->ai", -4//1, [bs_vo, d_ov, s₂_vo])
    update_code!(func, ein"bi,ja,bj->ai", -4//1, [bs_vo, d_ov, s₂_vo])
    update_code!(func, ein"bj,ia,bj->ai", 8//1, [bs_vo, d_ov, s₂_vo])
    update_code!(func, ein"aj,ib,bj,->ai", -1//1, [bs_vo, d_ov, s₁_vo, γ₁])
    update_code!(func, ein"bi,ja,bj,->ai", -1//1, [bs_vo, d_ov, s₁_vo, γ₁])
    update_code!(func, ein"iabc,bj,cj->ai", 1//1, [L_ovvv, bs_vo, s₁_vo])
    update_code!(func, ein"iajk,bk,bj->ai", -1//1, [L_ovoo, bs_vo, s₁_vo])
    update_code!(func, ein"ijkb,aj,bk->ai", -1//1, [L_ooov, bs_vo, s₁_vo])
    update_code!(func, ein"bajc,bi,cj->ai", 1//1, [L_vvov, bs_vo, s₁_vo])
    update_code!(func, ein"ajbk,ij,bk->ai", -2//1, [bs_vovo, d_oo, s₂_vo])
    update_code!(func, ein"bicj,ba,cj->ai", 2//1, [bs_vovo, d_vv, s₂_vo])
    update_code!(func, ein"ib,ajck,bjck->ai", -2//1, [F_ov, bs2_vovo, s₂_vovo])
    update_code!(func, ein"ja,bick,bjck->ai", -2//1, [F_ov, bs2_vovo, s₂_vovo])
    update_code!(func, ein"iajb,ck,bjck->ai", 2//1, [L_ovov, bs_vo, s₁_vovo])
    update_code!(func, ein"iajb,ck,bkcj->ai", -1//1, [L_ovov, bs_vo, s₁_vovo])
    update_code!(func, ein"ibjc,ak,bkcj->ai", -1//1, [L_ovov, bs_vo, s₁_vovo])
    update_code!(func, ein"jakb,ci,bkcj->ai", -1//1, [L_ovov, bs_vo, s₁_vovo])
    update_code!(func, ein"ajbk,ic,bkcj->ai", -2//1, [bs_vovo, d_ov, s₂_vovo])
    update_code!(func, ein"bicj,ka,bkcj->ai", -2//1, [bs_vovo, d_ov, s₂_vovo])
    update_code!(func, ein"bjck,ia,bjck->ai", 2//1, [bs_vovo, d_ov, s₂_vovo])
    update_code!(func, ein"ajbk,ic,bkcj->ai", -2//1, [bs2_vovo, d_ov, s₁_vovo])
    update_code!(func, ein"bicj,ka,bkcj->ai", -2//1, [bs2_vovo, d_ov, s₁_vovo])
    update_code!(func, ein"ajbk,ijbc,ck->ai", -2//1, [bs2_vovo, g_oovv, s₂_vo])
    update_code!(func, ein"ajbk,ijlk,bl->ai", 2//1, [bs2_vovo, g_oooo, s₂_vo])
    update_code!(func, ein"ajbk,icbk,cj->ai", -2//1, [bs2_vovo, g_ovvo, s₂_vo])
    update_code!(func, ein"bicj,bacd,dj->ai", 2//1, [bs2_vovo, g_vvvv, s₂_vo])
    update_code!(func, ein"bicj,bakj,ck->ai", -2//1, [bs2_vovo, g_vvoo, s₂_vo])
    update_code!(func, ein"bicj,cjka,bk->ai", -2//1, [bs2_vovo, g_voov, s₂_vo])
    update_code!(func, ein"ajbk,ic,bkcj,->ai", -2//1, [bs_vovo, d_ov, t_vovo, γ₂])
    update_code!(func, ein"bicj,ka,bkcj,->ai", -2//1, [bs_vovo, d_ov, t_vovo, γ₂])
    update_code!(func, ein"ajbk,ic,bk,cj->ai", -2//1, [bs2_vovo, d_ov, s₁_vo, s₂_vo])
    update_code!(func, ein"ajbk,ic,cj,bk->ai", -4//1, [bs2_vovo, d_ov, s₁_vo, s₂_vo])
    update_code!(func, ein"bicj,ka,bk,cj->ai", -4//1, [bs2_vovo, d_ov, s₁_vo, s₂_vo])
    update_code!(func, ein"bicj,ka,cj,bk->ai", -2//1, [bs2_vovo, d_ov, s₁_vo, s₂_vo])
    update_code!(func, ein"ajbk,ic,bkcj,->ai", -4//1, [bs2_vovo, d_ov, s₁_vovo, γ₂])
    update_code!(func, ein"bicj,ka,bkcj,->ai", -4//1, [bs2_vovo, d_ov, s₁_vovo, γ₂])
    update_code!(func, ein"ajbk,ic,bkcj,->ai", -2//1, [bs2_vovo, d_ov, s₂_vovo, γ₁])
    update_code!(func, ein"bicj,ka,bkcj,->ai", -2//1, [bs2_vovo, d_ov, s₂_vovo, γ₁])
    update_code!(func, ein"iabc,bjdk,cjdk->ai", 2//1, [L_ovvv, bs2_vovo, s₂_vovo])
    update_code!(func, ein"iajk,bkcl,bjcl->ai", -2//1, [L_ovoo, bs2_vovo, s₂_vovo])
    update_code!(func, ein"ijkb,ajcl,bkcl->ai", -2//1, [L_ooov, bs2_vovo, s₂_vovo])
    update_code!(func, ein"bajc,bidk,cjdk->ai", 2//1, [L_vvov, bs2_vovo, s₂_vovo])
    update_code!(func, ein"ajbk,ijlc,blck->ai", 2//1, [bs2_vovo, g_ooov, s₂_vovo])
    update_code!(func, ein"ajbk,icbd,cjdk->ai", -2//1, [bs2_vovo, g_ovvv, s₂_vovo])
    update_code!(func, ein"ajbk,iclk,blcj->ai", 2//1, [bs2_vovo, g_ovoo, s₂_vovo])
    update_code!(func, ein"bicj,bakd,ckdj->ai", -2//1, [bs2_vovo, g_vvov, s₂_vovo])
    update_code!(func, ein"bicj,cdka,bkdj->ai", -2//1, [bs2_vovo, g_vvov, s₂_vovo])
    update_code!(func, ein"bicj,kalj,bkcl->ai", 2//1, [bs2_vovo, g_ovoo, s₂_vovo])
    update_code!(func, ein"ajbk,ijlc,bl,ck->ai", 2//1, [bs2_vovo, g_ooov, s₁_vo, s₁_vo])
    update_code!(func, ein"ajbk,icbd,cj,dk->ai", -2//1, [bs2_vovo, g_ovvv, s₁_vo, s₁_vo])
    update_code!(func, ein"ajbk,iclk,bl,cj->ai", 2//1, [bs2_vovo, g_ovoo, s₁_vo, s₁_vo])
    update_code!(func, ein"bicj,bakd,ck,dj->ai", -2//1, [bs2_vovo, g_vvov, s₁_vo, s₁_vo])
    update_code!(func, ein"bicj,cdka,bk,dj->ai", -2//1, [bs2_vovo, g_vvov, s₁_vo, s₁_vo])
    update_code!(func, ein"bicj,kalj,bk,cl->ai", 2//1, [bs2_vovo, g_ovoo, s₁_vo, s₁_vo])
    update_code!(func, ein"iajb,ckdl,bk,cjdl->ai", -2//1, [L_ovov, bs2_vovo, s₁_vo, s₁_vovo])
    update_code!(func, ein"iajb,ckdl,cj,bkdl->ai", -2//1, [L_ovov, bs2_vovo, s₁_vo, s₁_vovo])
    update_code!(func, ein"ibjc,akdl,bk,cjdl->ai", -2//1, [L_ovov, bs2_vovo, s₁_vo, s₁_vovo])
    update_code!(func, ein"ibjc,akdl,cj,bkdl->ai", -2//1, [L_ovov, bs2_vovo, s₁_vo, s₁_vovo])
    update_code!(func, ein"jakb,cidl,bk,cjdl->ai", -2//1, [L_ovov, bs2_vovo, s₁_vo, s₁_vovo])
    update_code!(func, ein"jakb,cidl,cj,bkdl->ai", -2//1, [L_ovov, bs2_vovo, s₁_vo, s₁_vovo])
    update_code!(func, ein"iajb,ckdl,bk,cjdl->ai", -2//1, [L_ovov, bs2_vovo, s₂_vo, t_vovo])
    update_code!(func, ein"iajb,ckdl,cj,bkdl->ai", -2//1, [L_ovov, bs2_vovo, s₂_vo, t_vovo])
    update_code!(func, ein"ibjc,akdl,cj,bkdl->ai", -2//1, [L_ovov, bs2_vovo, s₂_vo, t_vovo])
    update_code!(func, ein"jakb,cidl,bk,cjdl->ai", -2//1, [L_ovov, bs2_vovo, s₂_vo, t_vovo])
    update_code!(func, ein"ajbk,icld,bl,cjdk->ai", 2//1, [bs2_vovo, g_ovov, s₁_vo, s₁_vovo])
    update_code!(func, ein"ajbk,icld,cj,bldk->ai", 2//1, [bs2_vovo, g_ovov, s₁_vo, s₁_vovo])
    update_code!(func, ein"ajbk,icld,dk,blcj->ai", 2//1, [bs2_vovo, g_ovov, s₁_vo, s₁_vovo])
    update_code!(func, ein"bicj,kald,bk,cldj->ai", 2//1, [bs2_vovo, g_ovov, s₁_vo, s₁_vovo])
    update_code!(func, ein"bicj,kald,cl,bkdj->ai", 2//1, [bs2_vovo, g_ovov, s₁_vo, s₁_vovo])
    update_code!(func, ein"bicj,kald,dj,bkcl->ai", 2//1, [bs2_vovo, g_ovov, s₁_vo, s₁_vovo])
    update_code!(func, ein"ajbk,icld,bl,cjdk->ai", 2//1, [bs2_vovo, g_ovov, s₂_vo, t_vovo])
    update_code!(func, ein"ajbk,icld,dj,bkcl->ai", 2//1, [bs2_vovo, g_ovov, s₂_vo, t_vovo])
    update_code!(func, ein"ajbk,icld,dk,blcj->ai", 2//1, [bs2_vovo, g_ovov, s₂_vo, t_vovo])
    update_code!(func, ein"bicj,kald,bl,cjdk->ai", 2//1, [bs2_vovo, g_ovov, s₂_vo, t_vovo])
    update_code!(func, ein"bicj,kald,cl,bkdj->ai", 2//1, [bs2_vovo, g_ovov, s₂_vo, t_vovo])
    update_code!(func, ein"bicj,kald,dj,bkcl->ai", 2//1, [bs2_vovo, g_ovov, s₂_vo, t_vovo])
    update_code!(func, ein"ajbk,icld,cj,bkdl->ai", -2//1, [bs2_vovo, g_ovov, s₂_vo, u_vovo])
    update_code!(func, ein"bicj,kald,bk,cjdl->ai", -2//1, [bs2_vovo, g_ovov, s₂_vo, u_vovo])
    finalize_eT_function(func, "jacobian_transpose_t1", "qed_ccsd_2")
end

