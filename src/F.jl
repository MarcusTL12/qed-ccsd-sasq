
function F_matrix(H, op)
    F_LR = SASQ.Expression(0)

    for i in 1:2, j in 1:2
        F_LR += simplify(hf_expectation_value(L_mu[i] * R_mu[j]))
    end
end
