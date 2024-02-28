
h = ∑(real_tensor("h", 1, 2) * E(1, 2), 1:2)
g = 1 // 2 * ∑(psym_tensor("g", 1, 2, 3, 4) * e(1, 2, 3, 4), 1:4) |> simplify

H_ω = real_tensor("ω") * b' * b
H_Eb = ∑(real_tensor("d", 1, 2) * E(1, 2) * (b' + b), 1:2)
H_b = -real_tensor("⟨d⟩") * (b' + b)

H = simplify(h + g + H_ω + H_Eb)

trans = translate(OccupiedOrbital => [2, 4], VirtualOrbital => [1, 3])

function omega(proj, op, n)
    simplify_heavy(
        hf_expectation_value(simplify(proj * bch(op, T, n)))
    )
end

function omega_t1()
    println("\n\nh: Ω0_ai += \n", (omega(deex_braop(1, 2), h, 2), trans))
    println("\n\ng: Ω0_ai += \n", (omega(deex_braop(1, 2), g, 2), trans))
    println("\n\nω: Ω0_ai += \n", (omega(deex_braop(1, 2), H_ω, 2), trans))
    println("\n\ned:Ω0_ai += \n", (omega(deex_braop(1, 2), H_Eb, 2), trans))
    println("\n\nd: Ω0_ai += \n", (omega(deex_braop(1, 2), H_b, 2), trans))
end

function omega_t2()
    println("\n\nh: Ω0_aibj += \n", (omega(deex_braop(1, 2, 3, 4), h, 2), trans))
    println("\n\ng: Ω0_aibj += \n", (omega(deex_braop(1, 2, 3, 4), g, 2), trans))
    println("\n\nω: Ω0_aibj += \n", (omega(deex_braop(1, 2, 3, 4), H_ω, 2), trans))
    println("\n\ned:Ω0_aibj += \n", (omega(deex_braop(1, 2, 3, 4), H_Eb, 2), trans))
    println("\n\nd: Ω0_aibj += \n", (omega(deex_braop(1, 2, 3, 4), H_b, 2), trans))
end

function omega_s0()
    println("\n\nh: Ω1 += \n", (omega(b, h, 2), trans))
    println("\n\ng: Ω1 += \n", (omega(b, g, 2), trans))
    println("\n\nω: Ω1 += \n", (omega(b, H_ω, 2), trans))
    println("\n\ned:Ω1 += \n", (omega(b, H_Eb, 2), trans))
    println("\n\nd: Ω1 += \n", (omega(b, H_b, 2), trans))
end

function omega_s1()
    println("\n\nh: Ω1_ai += \n", (omega(b * deex_braop(1, 2), h, 2), trans))
    println("\n\ng: Ω1_ai += \n", (omega(b * deex_braop(1, 2), g, 2), trans))
    println("\n\nω: Ω1_ai += \n", (omega(b * deex_braop(1, 2), H_ω, 2), trans))
    println("\n\ned:Ω1_ai += \n", (omega(b * deex_braop(1, 2), H_Eb, 2), trans))
    println("\n\nd: Ω1_ai += \n", (omega(b * deex_braop(1, 2), H_b, 2), trans))
end

function omega_s2()
    println("\n\nh: Ω1_aibj += \n", (omega(b * deex_braop(1, 2, 3, 4), h, 2), trans))
    println("\n\ng: Ω1_aibj += \n", (omega(b * deex_braop(1, 2, 3, 4), g, 2), trans))
    println("\n\nω: Ω1_aibj += \n", (omega(b * deex_braop(1, 2, 3, 4), H_ω, 2), trans))
    println("\n\ned:Ω1_aibj += \n", (omega(b * deex_braop(1, 2, 3, 4), H_Eb, 3), trans))
    println("\n\nd: Ω1_aibj += \n", (omega(b * deex_braop(1, 2, 3, 4), H_b, 2), trans))
end
