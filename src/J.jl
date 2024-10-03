
function J(right_op)
    J_ex = SASQ.Expression(0)

    for i in 1:2, j in 1:2
        J_ex += simplify(hf_expectation_value(L_mu[i] * R_mu[j] * right_op))
    end

    simplify(J_ex)
end

function J0_ai()
    println("J0_ai =")
    J(ex_ketop(1, 2))
end

function J0_aibj()
    println("J0_aibj =")
    J(ex_ketop(1, 2, 3, 4))
end

function J1()
    println("J1 =")
    J(b')
end

function J1_ai()
    println("J1_ai =")
    J(b' * ex_ketop(1, 2))
end

function J1_aibj()
    println("J1_aibj =")
    J(b' * ex_ketop(1, 2, 3, 4))
end
