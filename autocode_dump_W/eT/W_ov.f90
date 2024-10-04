   subroutine W_J_ov_terms_qed_ccsd(wf, W_J_ov, LJ_oo, LJ_ov, LJ_tr, LJ_vo, LJ_vv, Ls1, Ls_vo, Ls_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, Ru_vovo, Rv_vovo, s_vo, s_vovo, t_vovo, u_vovo, v_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_v), intent(inout) :: W_J_ov
!
      real(dp), intent(in) :: Ls1, Rs
      real(dp), dimension(wf%eri_t1%n_J), intent(in) :: LJ_tr
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_o), intent(in) :: LJ_oo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_v), intent(in) :: LJ_ov
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_o), intent(in) :: LJ_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: LJ_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rt_vovo, Ru_vovo, Rv_vovo, s_vovo, t_vovo, u_vovo, v_vovo
!
      real(dp) :: X29, X51, X113, X158, X206, X283, X439, X498
      real(dp), dimension(:), allocatable :: X5, X14, X21, X54, X63, X66, X75, X85, X98, X126, X130, X143, X170, X189, X218, X237, X286, X321, X324, X359, X591, X595, X612, X640, X644, X647, X676, X695, X736, X752
      real(dp), dimension(:,:), allocatable :: X1, X3, X6, X9, X10, X13, X15, X20, X22, X23, X24, X25, X26, X27, X28, X30, X31, X38, X45, X46, X48, X49, X53, X55, X57, X60, X62, X64, X65, X67, X68, X71, X74, X76, X77, X81, X83, X84, X86, X87, X90, X95, X97, X99, X100, X101, X105, X109, X118, X119, X125, X127, X128, X129, X131, X132, X135, X139, X142, X144, X145, X146, X150, X154, X161, X165, X169, X171, X172, X174, X176, X180, X183, X187, X188, X190, X191, X192, X193, X194, X195, X196, X197, X198, X199, X200, X201, X202, X203, X204, X205, X207, X208, X209, X210, X211, X212, X214, X215, X217, X219, X229, X236, X238, X239, X255, X256, X261, X268, X271, X272, X274, X275, X277, X278, X280, X281, X285, X287, X297, X305, X320, X322, X323, X325, X326, X350, X358, X360, X361, X362, X367, X374, X419, X427, X431, X435, X478, X486, X490, X494, X501, X505, X509, X513, X525, X529, X533, X537, X565, X573, X577, X581, X589, X590, X592, X593, X594, X596, X597, X598, X601, X602, X605, X606, X609, X610, X611, X630, X631, X633, X634, X637, X638, X639, X641, X642, X643, X645, X646, X648, X649, X650, X669, X670, X673, X674, X675, X693, X694, X696, X708, X709, X733, X734, X735, X737, X738, X739, X742, X743, X748, X749, X751, X753, X754, X755, X756, X757, X761, X762, X766, X767
      real(dp), dimension(:,:,:), allocatable :: X2, X4, X7, X8, X11, X12, X16, X17, X18, X19, X32, X33, X34, X35, X39, X40, X41, X42, X47, X50, X52, X56, X58, X59, X61, X69, X70, X72, X73, X78, X79, X80, X82, X88, X89, X91, X92, X93, X94, X96, X102, X103, X104, X106, X107, X108, X110, X111, X112, X114, X115, X116, X117, X120, X122, X124, X133, X134, X136, X137, X138, X140, X141, X147, X148, X149, X151, X152, X153, X155, X156, X157, X159, X160, X162, X163, X164, X166, X167, X168, X173, X175, X177, X178, X179, X181, X182, X184, X185, X186, X213, X216, X222, X227, X228, X230, X240, X241, X246, X254, X257, X258, X259, X260, X262, X263, X264, X273, X276, X279, X282, X284, X290, X295, X296, X298, X304, X306, X314, X319, X327, X328, X333, X341, X349, X351, X352, X357, X363, X364, X365, X366, X368, X369, X370, X377, X378, X379, X418, X420, X421, X422, X425, X428, X429, X430, X432, X433, X434, X436, X437, X438, X440, X441, X477, X479, X480, X481, X485, X487, X488, X489, X491, X492, X493, X495, X496, X497, X499, X500, X502, X503, X504, X506, X507, X508, X510, X511, X512, X514, X515, X516, X517, X518, X519, X520, X521, X522, X523, X524, X526, X527, X528, X530, X531, X532, X534, X535, X536, X538, X539, X540, X541, X542, X543, X544, X545, X546, X547, X548, X566, X567, X568, X572, X574, X575, X576, X578, X579, X580, X582, X583, X584, X585, X586, X587, X588, X599, X600, X603, X604, X607, X608, X616, X618, X622, X627, X628, X629, X632, X635, X636, X651, X652, X656, X658, X661, X666, X668, X671, X672, X679, X684, X686, X700, X705, X706, X707, X710, X720, X723, X728, X731, X740, X741, X744, X745, X746, X747, X750, X758, X759, X760, X763, X764, X765, X768, X769, X770
      real(dp), dimension(:,:,:,:), allocatable :: X36, X37, X43, X44, X121, X123, X220, X221, X223, X224, X225, X226, X231, X232, X233, X234, X235, X242, X243, X244, X245, X247, X248, X249, X250, X251, X252, X253, X265, X266, X267, X269, X270, X288, X289, X291, X292, X293, X294, X299, X300, X301, X302, X303, X307, X308, X309, X310, X311, X312, X313, X315, X316, X317, X318, X329, X330, X331, X332, X334, X335, X336, X337, X338, X339, X340, X342, X343, X344, X345, X346, X347, X348, X353, X354, X355, X356, X371, X372, X373, X375, X376, X380, X381, X382, X383, X384, X385, X386, X387, X388, X389, X390, X391, X392, X393, X394, X395, X396, X397, X398, X399, X400, X401, X402, X403, X404, X405, X406, X407, X408, X409, X410, X411, X412, X413, X414, X415, X416, X417, X423, X424, X426, X442, X443, X444, X445, X446, X447, X448, X449, X450, X451, X452, X453, X454, X455, X456, X457, X458, X459, X460, X461, X462, X463, X464, X465, X466, X467, X468, X469, X470, X471, X472, X473, X474, X475, X476, X482, X483, X484, X549, X550, X551, X552, X553, X554, X555, X556, X557, X558, X559, X560, X561, X562, X563, X564, X569, X570, X571, X613, X614, X615, X617, X619, X620, X621, X623, X624, X625, X626, X653, X654, X655, X657, X659, X660, X662, X663, X664, X665, X667, X677, X678, X680, X681, X682, X683, X685, X687, X688, X689, X690, X691, X692, X697, X698, X699, X701, X702, X703, X704, X711, X712, X713, X714, X715, X716, X717, X718, X719, X721, X722, X724, X725, X726, X727, X729, X730, X732
!
      real(dp), external :: ddot
!
      call mem%alloc(X1, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X1, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         four*Ls1, &
         LJ_tr, 1, &
         X1, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X2, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         -two*Ls1, &
         X2, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X3, &
         wf%n_v)
!
      call mem%alloc(X4, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(LJ_vo, X4, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X4, &
         wf%eri_t1%n_J*wf%n_o, &
         X3, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X3)
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         Ls_vo, 1, &
         zero, &
         X5, 1)
!
      call mem%alloc(X6, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X6, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X5, 1, &
         X6, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X5)
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X7, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X8, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two*Ls1, &
         X7, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X8, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X7)
      call add_132_to_123(one, X8, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X9, 1)
!
      call mem%alloc(X10, wf%n_o, wf%n_v)
      call sort_to_21(X9, X10, wf%n_v, wf%n_o)
      call mem%dealloc(X9)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X10, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X11, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X12, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two*Ls1*Rs, &
         X11, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X12, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X11)
      call add_132_to_123(one, X12, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X13, wf%n_v, wf%n_o)
      call mem%alloc(X14, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         four*Ls1, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X13, 1, &
         zero, &
         X14, 1)
!
      call mem%dealloc(X13)
      call mem%alloc(X15, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X15, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X14, 1, &
         X15, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X14)
      call mem%dealloc(X15)
      call mem%alloc(X16, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two*Ls1, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X16, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X17, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X16, X17, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X16)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X17, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two*Ls1, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X18, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X19, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X18, X19, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X18)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X19, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X20, wf%n_v, wf%n_o)
      call mem%alloc(X21, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         four*Ls1, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X20, 1, &
         zero, &
         X21, 1)
!
      call mem%dealloc(X20)
      call mem%alloc(X22, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X22, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X21, 1, &
         X22, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X21)
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two*Rs, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X23, 1)
!
      call mem%alloc(X24, wf%n_o, wf%n_v)
      call sort_to_21(X23, X24, wf%n_v, wf%n_o)
      call mem%dealloc(X23)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X24, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X25, &
         wf%n_o)
!
      call mem%alloc(X26, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X25, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X26, &
         wf%n_o)
!
      call mem%dealloc(X25)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X26, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X26)
      call mem%alloc(X27, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X27, &
         wf%n_o)
!
      call mem%alloc(X28, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X27, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X28, &
         wf%n_o)
!
      call mem%dealloc(X27)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X28, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X28)
      X29 = four * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rt_vo, 1)
      call mem%alloc(X30, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X30, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         X29, &
         LJ_tr, 1, &
         X30, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X30)
      call mem%alloc(X31, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X31, 1)
!
      call mem%alloc(X32, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X32, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X32, &
         wf%eri_t1%n_J*wf%n_o, &
         X31, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X31)
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call sort_to_132(LJ_vv, X33, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%alloc(X34, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X33, &
         wf%n_v*wf%eri_t1%n_J, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X34, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X33)
      call mem%alloc(X35, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X34, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X35, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X34)
      call add_132_to_123(one, X35, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X35)
      call mem%alloc(X36, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rv_vovo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X36, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X37, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X36, X37, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X36)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X37, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X37)
      call mem%alloc(X38, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -Rs, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X38, 1)
!
      call mem%alloc(X39, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X39, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X39, &
         wf%eri_t1%n_J*wf%n_o, &
         X38, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X38)
      call mem%dealloc(X39)
      call mem%alloc(X40, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call sort_to_132(LJ_vv, X40, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%alloc(X41, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         Rs, &
         X40, &
         wf%n_v*wf%eri_t1%n_J, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X41, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X40)
      call mem%alloc(X42, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X41, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X42, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X41)
      call add_132_to_123(one, X42, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X42)
      call mem%alloc(X43, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -Rs, &
         u_vovo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X43, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X44, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X43, X44, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X43)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X44, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X44)
      call mem%alloc(X45, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X45, &
         wf%n_o)
!
      call mem%alloc(X46, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X45, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X46, &
         wf%n_o)
!
      call mem%dealloc(X45)
      call mem%alloc(X47, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X47, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X47, &
         wf%eri_t1%n_J*wf%n_o, &
         X46, &
         wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X46)
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X48, &
         wf%n_o)
!
      call mem%alloc(X49, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X48, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X49, &
         wf%n_o)
!
      call mem%dealloc(X48)
      call mem%alloc(X50, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X50, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X50, &
         wf%eri_t1%n_J*wf%n_o, &
         X49, &
         wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X49)
      call mem%dealloc(X50)
      X51 = -two * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rt_vo, 1)
      call mem%alloc(X52, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X52, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         X51, &
         X52, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X52)
      call mem%alloc(X53, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         two, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X53, &
         wf%n_v)
!
      call mem%alloc(X54, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X53, 1, &
         zero, &
         X54, 1)
!
      call mem%dealloc(X53)
      call mem%alloc(X55, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X55, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X54, 1, &
         X55, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X54)
      call mem%dealloc(X55)
      call mem%alloc(X56, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         zero, &
         X56, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X57, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X57, &
         wf%n_v)
!
      call mem%alloc(X58, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X56, X58, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X56)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X58, &
         wf%eri_t1%n_J*wf%n_o, &
         X57, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X58)
      call mem%dealloc(X57)
      call mem%alloc(X59, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X59, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X60, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X60, &
         wf%n_v)
!
      call mem%alloc(X61, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X59, X61, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X59)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X61, &
         wf%eri_t1%n_J*wf%n_o, &
         X60, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X61)
      call mem%dealloc(X60)
      call mem%alloc(X62, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         two, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X62, &
         wf%n_v)
!
      call mem%alloc(X63, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X62, 1, &
         zero, &
         X63, 1)
!
      call mem%dealloc(X62)
      call mem%alloc(X64, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X64, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X63, 1, &
         X64, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X63)
      call mem%dealloc(X64)
      call mem%alloc(X65, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         s_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X65, &
         wf%n_o)
!
      call mem%alloc(X66, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X65, 1, &
         zero, &
         X66, 1)
!
      call mem%dealloc(X65)
      call mem%alloc(X67, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X67, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X66, 1, &
         X67, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X66)
      call mem%dealloc(X67)
      call mem%alloc(X68, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X68, &
         wf%n_o)
!
      call mem%alloc(X69, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X68, &
         wf%n_o, &
         zero, &
         X69, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X68)
      call mem%alloc(X70, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X69, X70, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X69)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X70, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X70)
      call mem%alloc(X71, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X71, &
         wf%n_o)
!
      call mem%alloc(X72, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X71, &
         wf%n_o, &
         zero, &
         X72, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X71)
      call mem%alloc(X73, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X72, X73, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X72)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X73, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X73)
      call mem%alloc(X74, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Rt_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X74, &
         wf%n_o)
!
      call mem%alloc(X75, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X74, 1, &
         zero, &
         X75, 1)
!
      call mem%dealloc(X74)
      call mem%alloc(X76, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X76, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X75, 1, &
         X76, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X75)
      call mem%dealloc(X76)
      call mem%alloc(X77, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X77, &
         wf%n_v)
!
      call mem%alloc(X78, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(LJ_vo, X78, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X78, &
         wf%eri_t1%n_J*wf%n_o, &
         X77, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X77)
      call mem%dealloc(X78)
      call mem%alloc(X79, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X79, &
         wf%eri_t1%n_J)
!
      call mem%alloc(X80, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X79, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X80, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X79)
      call add_132_to_123(one, X80, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X80)
      call mem%alloc(X81, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -Rs, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X81, &
         wf%n_v)
!
      call mem%alloc(X82, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(LJ_vo, X82, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X82, &
         wf%eri_t1%n_J*wf%n_o, &
         X81, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X81)
      call mem%dealloc(X82)
      call mem%alloc(X83, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X83, 1)
!
      call mem%alloc(X84, wf%n_o, wf%n_v)
      call sort_to_21(X83, X84, wf%n_v, wf%n_o)
      call mem%dealloc(X83)
      call mem%alloc(X85, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X84, 1, &
         zero, &
         X85, 1)
!
      call mem%dealloc(X84)
      call mem%alloc(X86, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X86, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X85, 1, &
         X86, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X85)
      call mem%dealloc(X86)
      call mem%alloc(X87, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X87, 1)
!
      call mem%alloc(X88, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X87, &
         wf%n_v, &
         zero, &
         X88, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X87)
      call mem%alloc(X89, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X88, X89, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X88)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X89, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X89)
      call mem%alloc(X90, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X90, &
         wf%n_v)
!
      call mem%alloc(X91, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X91, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X92, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X91, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X92, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X91)
      call mem%alloc(X93, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X92, X93, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X92)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X93, &
         wf%eri_t1%n_J*wf%n_o, &
         X90, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X90)
      call mem%dealloc(X93)
      call mem%alloc(X94, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X94, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X95, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X95, 1)
!
      call mem%alloc(X96, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X94, X96, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X94)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X96, &
         wf%eri_t1%n_J*wf%n_o, &
         X95, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X96)
      call mem%dealloc(X95)
      call mem%alloc(X97, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X97, wf%n_v, wf%n_o)
      call mem%alloc(X98, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X97, 1, &
         zero, &
         X98, 1)
!
      call mem%dealloc(X97)
      call mem%alloc(X99, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X99, 1)
!
      call mem%alloc(X100, wf%n_o, wf%n_v)
      call sort_to_21(X99, X100, wf%n_v, wf%n_o)
      call mem%dealloc(X99)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X98, 1, &
         X100, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X98)
      call mem%dealloc(X100)
      call mem%alloc(X101, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X101, &
         wf%n_v)
!
      call mem%alloc(X102, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X101, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X102, &
         wf%n_v)
!
      call mem%dealloc(X101)
      call mem%alloc(X103, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X102, X103, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X102)
      call mem%alloc(X104, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X103, &
         wf%n_v*wf%n_o, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X104, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X103)
      call add_132_to_123(one, X104, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X104)
      call mem%alloc(X105, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X105, &
         wf%n_o)
!
      call mem%alloc(X106, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X106, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X107, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X106, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X107, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X106)
      call mem%alloc(X108, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X107, &
         wf%n_v*wf%eri_t1%n_J, &
         X105, &
         wf%n_o, &
         zero, &
         X108, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X105)
      call mem%dealloc(X107)
      call add_132_to_123(one, X108, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X108)
      call mem%alloc(X109, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X109, &
         wf%n_o)
!
      call mem%alloc(X110, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X110, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X111, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X110, &
         wf%n_v*wf%eri_t1%n_J, &
         X109, &
         wf%n_o, &
         zero, &
         X111, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X109)
      call mem%dealloc(X110)
      call mem%alloc(X112, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X111, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X112, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X111)
      call add_132_to_123(one, X112, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X112)
      X113 = two * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rs_vo, 1)
      call mem%alloc(X114, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X114, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X115, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         X113, &
         X114, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X115, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X114)
      call add_132_to_123(one, X115, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X115)
      call mem%alloc(X116, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X116, &
         wf%eri_t1%n_J)
!
      call mem%alloc(X117, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X116, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X117, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X116)
      call add_132_to_123(one, X117, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X117)
      call mem%alloc(X118, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X118, 1)
!
      call mem%alloc(X119, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X118, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X119, &
         wf%n_v)
!
      call mem%dealloc(X118)
      call mem%alloc(X120, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(LJ_vo, X120, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X120, &
         wf%eri_t1%n_J*wf%n_o, &
         X119, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X119)
      call mem%dealloc(X120)
      call mem%alloc(X121, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X121, &
         wf%n_o)
!
      call mem%alloc(X122, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         X121, &
         wf%n_o**2, &
         zero, &
         X122, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X121)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X122, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X122)
      call mem%alloc(X123, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X123, &
         wf%n_o)
!
      call mem%alloc(X124, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         X123, &
         wf%n_o**2, &
         zero, &
         X124, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X123)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X124, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X124)
      call mem%alloc(X125, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X125, 1)
!
      call mem%alloc(X126, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         X125, 1, &
         zero, &
         X126, 1)
!
      call mem%dealloc(X125)
      call mem%alloc(X127, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X127, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X126, 1, &
         X127, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X126)
      call mem%dealloc(X127)
      call mem%alloc(X128, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X128, 1)
!
      call mem%alloc(X129, wf%n_o, wf%n_v)
      call sort_to_21(X128, X129, wf%n_v, wf%n_o)
      call mem%dealloc(X128)
      call mem%alloc(X130, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X129, 1, &
         zero, &
         X130, 1)
!
      call mem%dealloc(X129)
      call mem%alloc(X131, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X131, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X130, 1, &
         X131, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X130)
      call mem%dealloc(X131)
      call mem%alloc(X132, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X132, 1)
!
      call mem%alloc(X133, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X132, &
         wf%n_v, &
         zero, &
         X133, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X132)
      call mem%alloc(X134, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X133, X134, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X133)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X134, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X134)
      call mem%alloc(X135, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X135, &
         wf%n_v)
!
      call mem%alloc(X136, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X136, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X137, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X136, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X137, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X136)
      call mem%alloc(X138, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X137, X138, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X137)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X138, &
         wf%eri_t1%n_J*wf%n_o, &
         X135, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X135)
      call mem%dealloc(X138)
      call mem%alloc(X139, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X139, 1)
!
      call mem%alloc(X140, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X140, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X141, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X140, X141, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X140)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X141, &
         wf%eri_t1%n_J*wf%n_o, &
         X139, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X139)
      call mem%dealloc(X141)
      call mem%alloc(X142, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X142, wf%n_v, wf%n_o)
      call mem%alloc(X143, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X142, 1, &
         zero, &
         X143, 1)
!
      call mem%dealloc(X142)
      call mem%alloc(X144, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X144, 1)
!
      call mem%alloc(X145, wf%n_o, wf%n_v)
      call sort_to_21(X144, X145, wf%n_v, wf%n_o)
      call mem%dealloc(X144)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X143, 1, &
         X145, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X143)
      call mem%dealloc(X145)
      call mem%alloc(X146, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X146, &
         wf%n_v)
!
      call mem%alloc(X147, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X146, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X147, &
         wf%n_v)
!
      call mem%dealloc(X146)
      call mem%alloc(X148, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X147, X148, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X147)
      call mem%alloc(X149, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X148, &
         wf%n_v*wf%n_o, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X149, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X148)
      call add_132_to_123(one, X149, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X149)
      call mem%alloc(X150, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X150, &
         wf%n_o)
!
      call mem%alloc(X151, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X151, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X152, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X151, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X152, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X151)
      call mem%alloc(X153, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X152, &
         wf%n_v*wf%eri_t1%n_J, &
         X150, &
         wf%n_o, &
         zero, &
         X153, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X150)
      call mem%dealloc(X152)
      call add_132_to_123(one, X153, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X153)
      call mem%alloc(X154, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X154, &
         wf%n_o)
!
      call mem%alloc(X155, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X155, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X156, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X155, &
         wf%n_v*wf%eri_t1%n_J, &
         X154, &
         wf%n_o, &
         zero, &
         X156, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X154)
      call mem%dealloc(X155)
      call mem%alloc(X157, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X156, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X157, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X156)
      call add_132_to_123(one, X157, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X157)
      X158 = two * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rt_vo, 1)
      call mem%alloc(X159, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X159, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X160, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         X158, &
         X159, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X160, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X159)
      call add_132_to_123(one, X160, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X160)
      call mem%alloc(X161, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X161, &
         wf%n_o)
!
      call mem%alloc(X162, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X162, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X163, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X162, &
         wf%n_v*wf%eri_t1%n_J, &
         X161, &
         wf%n_o, &
         zero, &
         X163, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X161)
      call mem%dealloc(X162)
      call mem%alloc(X164, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X163, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X164, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X163)
      call add_132_to_123(one, X164, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X164)
      call mem%alloc(X165, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X165, &
         wf%n_v)
!
      call mem%alloc(X166, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X165, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X166, &
         wf%n_v)
!
      call mem%dealloc(X165)
      call mem%alloc(X167, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X166, X167, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X166)
      call mem%alloc(X168, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X167, &
         wf%n_v*wf%n_o, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X168, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X167)
      call add_132_to_123(one, X168, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X168)
      call mem%alloc(X169, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X169, wf%n_v, wf%n_o)
      call mem%alloc(X170, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X169, 1, &
         zero, &
         X170, 1)
!
      call mem%dealloc(X169)
      call mem%alloc(X171, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X171, 1)
!
      call mem%alloc(X172, wf%n_o, wf%n_v)
      call sort_to_21(X171, X172, wf%n_v, wf%n_o)
      call mem%dealloc(X171)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X170, 1, &
         X172, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X170)
      call mem%dealloc(X172)
      call mem%alloc(X173, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X173, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X174, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X174, 1)
!
      call mem%alloc(X175, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X173, X175, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X173)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X175, &
         wf%eri_t1%n_J*wf%n_o, &
         X174, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X175)
      call mem%dealloc(X174)
      call mem%alloc(X176, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X176, &
         wf%n_o)
!
      call mem%alloc(X177, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X177, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X178, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X177, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X178, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X177)
      call mem%alloc(X179, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X178, &
         wf%n_v*wf%eri_t1%n_J, &
         X176, &
         wf%n_o, &
         zero, &
         X179, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X176)
      call mem%dealloc(X178)
      call add_132_to_123(one, X179, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X179)
      call mem%alloc(X180, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X180, 1)
!
      call mem%alloc(X181, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X180, &
         wf%n_v, &
         zero, &
         X181, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X180)
      call mem%alloc(X182, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X181, X182, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X181)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X182, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X182)
      call mem%alloc(X183, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X183, &
         wf%n_v)
!
      call mem%alloc(X184, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X184, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X185, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X184, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X185, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X184)
      call mem%alloc(X186, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X185, X186, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X185)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X186, &
         wf%eri_t1%n_J*wf%n_o, &
         X183, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X183)
      call mem%dealloc(X186)
      call mem%alloc(X187, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X187, 1)
!
      call mem%alloc(X188, wf%n_o, wf%n_v)
      call sort_to_21(X187, X188, wf%n_v, wf%n_o)
      call mem%dealloc(X187)
      call mem%alloc(X189, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X188, 1, &
         zero, &
         X189, 1)
!
      call mem%dealloc(X188)
      call mem%alloc(X190, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X190, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X189, 1, &
         X190, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X189)
      call mem%dealloc(X190)
      call mem%alloc(X191, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X191, &
         wf%n_o)
!
      call mem%alloc(X192, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X191, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X192, &
         wf%n_o)
!
      call mem%dealloc(X191)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X192, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X192)
      call mem%alloc(X193, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X193, &
         wf%n_v)
!
      call mem%alloc(X194, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         X193, &
         wf%n_v, &
         zero, &
         X194, &
         wf%n_o)
!
      call mem%dealloc(X193)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X194, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X194)
      call mem%alloc(X195, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X195, 1)
!
      call mem%alloc(X196, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X195, 1, &
         zero, &
         X196, 1)
!
      call mem%dealloc(X195)
      call mem%alloc(X197, wf%n_o, wf%n_v)
      call sort_to_21(X196, X197, wf%n_v, wf%n_o)
      call mem%dealloc(X196)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X197, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X197)
      call mem%alloc(X198, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X198, &
         wf%n_o)
!
      call mem%alloc(X199, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X198, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X199, &
         wf%n_o)
!
      call mem%dealloc(X198)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X199, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X199)
      call mem%alloc(X200, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X200, &
         wf%n_v)
!
      call mem%alloc(X201, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X200, &
         wf%n_v, &
         zero, &
         X201, &
         wf%n_o)
!
      call mem%dealloc(X200)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X201, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X201)
      call mem%alloc(X202, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X202, &
         wf%n_v)
!
      call mem%alloc(X203, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X202, &
         wf%n_v, &
         zero, &
         X203, &
         wf%n_o)
!
      call mem%dealloc(X202)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X203, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X203)
      call mem%alloc(X204, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X204, &
         wf%n_o)
!
      call mem%alloc(X205, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X204, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X205, &
         wf%n_o)
!
      call mem%dealloc(X204)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X205, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X205)
      X206 = two * ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rt_vovo, 1)
      call mem%alloc(X207, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X207, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         X206, &
         LJ_tr, 1, &
         X207, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X207)
      call mem%alloc(X208, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X208, 1)
!
      call mem%alloc(X209, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X208, 1, &
         zero, &
         X209, 1)
!
      call mem%dealloc(X208)
      call mem%alloc(X210, wf%n_o, wf%n_v)
      call sort_to_21(X209, X210, wf%n_v, wf%n_o)
      call mem%dealloc(X209)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X210, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X210)
      call mem%alloc(X211, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X211, &
         wf%n_o)
!
      call mem%alloc(X212, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X211, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X212, &
         wf%n_o)
!
      call mem%dealloc(X211)
      call mem%alloc(X213, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X213, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X213, &
         wf%eri_t1%n_J*wf%n_o, &
         X212, &
         wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X212)
      call mem%dealloc(X213)
      call mem%alloc(X214, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X214, &
         wf%n_v)
!
      call mem%alloc(X215, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         X214, &
         wf%n_v, &
         zero, &
         X215, &
         wf%n_o)
!
      call mem%dealloc(X214)
      call mem%alloc(X216, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X216, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X216, &
         wf%eri_t1%n_J*wf%n_o, &
         X215, &
         wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X215)
      call mem%dealloc(X216)
      call mem%alloc(X217, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X217, &
         wf%n_v)
!
      call mem%alloc(X218, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X217, 1, &
         zero, &
         X218, 1)
!
      call mem%dealloc(X217)
      call mem%alloc(X219, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X219, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X218, 1, &
         X219, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X218)
      call mem%dealloc(X219)
      call mem%alloc(X220, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X220, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X221, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X220, X221, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X220)
      call mem%alloc(X222, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X221, &
         wf%n_v**2, &
         zero, &
         X222, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X221)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X222, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X222)
      call mem%alloc(X223, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X223, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X224, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X224, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X225, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X223, &
         wf%n_v*wf%n_o, &
         X224, &
         wf%n_v*wf%n_o, &
         zero, &
         X225, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X223)
      call mem%dealloc(X224)
      call mem%alloc(X226, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X225, X226, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X225)
      call mem%alloc(X227, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X226, &
         wf%n_v**2, &
         zero, &
         X227, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X226)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X227, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X227)
      call mem%alloc(X228, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X228, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X229, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X229, &
         wf%n_v)
!
      call mem%alloc(X230, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X228, X230, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X228)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X230, &
         wf%eri_t1%n_J*wf%n_o, &
         X229, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X230)
      call mem%dealloc(X229)
      call mem%alloc(X231, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X231, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X232, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X231, X232, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X231)
      call mem%alloc(X233, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X233, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X234, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X232, &
         wf%n_o**2, &
         X233, &
         wf%n_v**2, &
         zero, &
         X234, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X232)
      call mem%dealloc(X233)
      call mem%alloc(X235, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1423(X234, X235, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X234)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X235, &
         wf%n_v**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X235)
      call mem%alloc(X236, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X236, &
         wf%n_o)
!
      call mem%alloc(X237, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X236, 1, &
         zero, &
         X237, 1)
!
      call mem%dealloc(X236)
      call mem%alloc(X238, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X238, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X237, 1, &
         X238, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X237)
      call mem%dealloc(X238)
      call mem%alloc(X239, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X239, &
         wf%n_o)
!
      call mem%alloc(X240, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X239, &
         wf%n_o, &
         zero, &
         X240, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X239)
      call mem%alloc(X241, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X240, X241, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X240)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X241, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X241)
      call mem%alloc(X242, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X242, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X243, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X243, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X244, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X243, &
         wf%n_v**2, &
         X242, &
         wf%n_v**2, &
         zero, &
         X244, &
         wf%n_o**2)
!
      call mem%dealloc(X242)
      call mem%dealloc(X243)
      call mem%alloc(X245, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X244, X245, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X244)
      call mem%alloc(X246, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X245, &
         wf%n_o**2, &
         zero, &
         X246, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X245)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X246, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X246)
      call mem%alloc(X247, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X247, &
         wf%n_o)
!
      call mem%alloc(X248, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X247, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X248, &
         wf%n_o**2)
!
      call mem%dealloc(X247)
      call mem%alloc(X249, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X248, X249, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X248)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X249, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X249)
      call mem%alloc(X250, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X250, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X251, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X251, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X252, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X250, &
         wf%n_v*wf%n_o, &
         X251, &
         wf%n_v*wf%n_o, &
         zero, &
         X252, &
         wf%n_o**2)
!
      call mem%dealloc(X250)
      call mem%dealloc(X251)
      call mem%alloc(X253, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X252, X253, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X252)
      call mem%alloc(X254, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X254, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X254, &
         wf%eri_t1%n_J, &
         X253, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X253)
      call mem%dealloc(X254)
      call mem%alloc(X255, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X255, 1)
!
      call mem%alloc(X256, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X255, 1, &
         zero, &
         X256, 1)
!
      call mem%dealloc(X255)
      call mem%alloc(X257, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X257, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X257, &
         wf%eri_t1%n_J*wf%n_o, &
         X256, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X256)
      call mem%dealloc(X257)
      call mem%alloc(X258, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X258, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X259, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X258, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X259, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X258)
      call mem%alloc(X260, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X259, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X260, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X259)
      call add_132_to_123(one, X260, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X260)
      call mem%alloc(X261, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X261, 1)
!
      call mem%alloc(X262, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call sort_to_132(LJ_vv, X262, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%alloc(X263, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X262, &
         wf%n_v*wf%eri_t1%n_J, &
         X261, &
         wf%n_v, &
         zero, &
         X263, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X261)
      call mem%dealloc(X262)
      call mem%alloc(X264, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X263, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X264, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X263)
      call add_132_to_123(one, X264, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X264)
      call mem%alloc(X265, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X265, &
         wf%n_o)
!
      call mem%alloc(X266, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X265, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X266, &
         wf%n_o**2)
!
      call mem%dealloc(X265)
      call mem%alloc(X267, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X266, X267, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X266)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X267, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X267)
      call mem%alloc(X268, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X268, 1)
!
      call mem%alloc(X269, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         u_vovo, &
         wf%n_v, &
         X268, &
         wf%n_v, &
         zero, &
         X269, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X268)
      call mem%alloc(X270, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X269, X270, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X269)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X270, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X270)
      call mem%alloc(X271, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X271, &
         wf%n_o)
!
      call mem%alloc(X272, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X271, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X272, &
         wf%n_o)
!
      call mem%dealloc(X271)
      call mem%alloc(X273, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X273, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X273, &
         wf%eri_t1%n_J*wf%n_o, &
         X272, &
         wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X272)
      call mem%dealloc(X273)
      call mem%alloc(X274, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X274, &
         wf%n_v)
!
      call mem%alloc(X275, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X274, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X275, &
         wf%n_v)
!
      call mem%dealloc(X274)
      call mem%alloc(X276, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X276, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X276, &
         wf%eri_t1%n_J*wf%n_o, &
         X275, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X275)
      call mem%dealloc(X276)
      call mem%alloc(X277, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X277, &
         wf%n_v)
!
      call mem%alloc(X278, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X277, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X278, &
         wf%n_v)
!
      call mem%dealloc(X277)
      call mem%alloc(X279, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X279, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X279, &
         wf%eri_t1%n_J*wf%n_o, &
         X278, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X278)
      call mem%dealloc(X279)
      call mem%alloc(X280, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X280, &
         wf%n_o)
!
      call mem%alloc(X281, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X280, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X281, &
         wf%n_o)
!
      call mem%dealloc(X280)
      call mem%alloc(X282, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X282, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X282, &
         wf%eri_t1%n_J*wf%n_o, &
         X281, &
         wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X281)
      call mem%dealloc(X282)
      X283 =  -ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rt_vovo, 1)
      call mem%alloc(X284, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X284, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         X283, &
         X284, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X284)
      call mem%alloc(X285, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X285, &
         wf%n_v)
!
      call mem%alloc(X286, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X285, 1, &
         zero, &
         X286, 1)
!
      call mem%dealloc(X285)
      call mem%alloc(X287, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X287, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X286, 1, &
         X287, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X286)
      call mem%dealloc(X287)
      call mem%alloc(X288, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X288, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X289, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X288, X289, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X288)
      call mem%alloc(X290, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X289, &
         wf%n_v**2, &
         zero, &
         X290, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X289)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X290, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X290)
      call mem%alloc(X291, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X291, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X292, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s_vovo, X292, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X293, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X291, &
         wf%n_v*wf%n_o, &
         X292, &
         wf%n_v*wf%n_o, &
         zero, &
         X293, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X291)
      call mem%dealloc(X292)
      call mem%alloc(X294, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X293, X294, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X293)
      call mem%alloc(X295, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X294, &
         wf%n_v**2, &
         zero, &
         X295, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X294)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X295, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X295)
      call mem%alloc(X296, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X296, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X297, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X297, &
         wf%n_v)
!
      call mem%alloc(X298, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X296, X298, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X296)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X298, &
         wf%eri_t1%n_J*wf%n_o, &
         X297, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X298)
      call mem%dealloc(X297)
      call mem%alloc(X299, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X299, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X300, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X299, X300, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X299)
      call mem%alloc(X301, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s_vovo, X301, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X302, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X300, &
         wf%n_o**2, &
         X301, &
         wf%n_v**2, &
         zero, &
         X302, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X300)
      call mem%dealloc(X301)
      call mem%alloc(X303, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1423(X302, X303, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X302)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X303, &
         wf%n_v**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X303)
      call mem%alloc(X304, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         zero, &
         X304, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X305, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X305, &
         wf%n_v)
!
      call mem%alloc(X306, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X304, X306, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X304)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X306, &
         wf%eri_t1%n_J*wf%n_o, &
         X305, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X306)
      call mem%dealloc(X305)
      call mem%alloc(X307, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X307, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X308, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X307, X308, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X307)
      call mem%alloc(X309, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X309, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X310, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X308, &
         wf%n_o**2, &
         X309, &
         wf%n_v**2, &
         zero, &
         X310, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X308)
      call mem%dealloc(X309)
      call mem%alloc(X311, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1423(X310, X311, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X310)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X311, &
         wf%n_v**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X311)
      call mem%alloc(X312, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X312, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X313, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X312, X313, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X312)
      call mem%alloc(X314, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X313, &
         wf%n_v**2, &
         zero, &
         X314, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X313)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X314, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X314)
      call mem%alloc(X315, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X315, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X316, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X316, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X317, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X315, &
         wf%n_v*wf%n_o, &
         X316, &
         wf%n_v*wf%n_o, &
         zero, &
         X317, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X315)
      call mem%dealloc(X316)
      call mem%alloc(X318, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X317, X318, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X317)
      call mem%alloc(X319, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X318, &
         wf%n_v**2, &
         zero, &
         X319, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X318)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X319, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X319)
      call mem%alloc(X320, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X320, &
         wf%n_v)
!
      call mem%alloc(X321, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X320, 1, &
         zero, &
         X321, 1)
!
      call mem%dealloc(X320)
      call mem%alloc(X322, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X322, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X321, 1, &
         X322, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X321)
      call mem%dealloc(X322)
      call mem%alloc(X323, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X323, &
         wf%n_o)
!
      call mem%alloc(X324, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X323, 1, &
         zero, &
         X324, 1)
!
      call mem%dealloc(X323)
      call mem%alloc(X325, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X325, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X324, 1, &
         X325, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X324)
      call mem%dealloc(X325)
      call mem%alloc(X326, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X326, &
         wf%n_o)
!
      call mem%alloc(X327, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X326, &
         wf%n_o, &
         zero, &
         X327, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X326)
      call mem%alloc(X328, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X327, X328, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X327)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X328, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X328)
      call mem%alloc(X329, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X329, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X330, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s_vovo, X330, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X331, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X330, &
         wf%n_v**2, &
         X329, &
         wf%n_v**2, &
         zero, &
         X331, &
         wf%n_o**2)
!
      call mem%dealloc(X329)
      call mem%dealloc(X330)
      call mem%alloc(X332, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X331, X332, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X331)
      call mem%alloc(X333, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X332, &
         wf%n_o**2, &
         zero, &
         X333, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X332)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X333, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X333)
      call mem%alloc(X334, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X334, &
         wf%n_o)
!
      call mem%alloc(X335, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X334, &
         wf%n_o**2, &
         s_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X335, &
         wf%n_o**2)
!
      call mem%dealloc(X334)
      call mem%alloc(X336, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X335, X336, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X335)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X336, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X336)
      call mem%alloc(X337, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X337, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X338, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s_vovo, X338, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X339, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X337, &
         wf%n_v*wf%n_o, &
         X338, &
         wf%n_v*wf%n_o, &
         zero, &
         X339, &
         wf%n_o**2)
!
      call mem%dealloc(X337)
      call mem%dealloc(X338)
      call mem%alloc(X340, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X339, X340, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X339)
      call mem%alloc(X341, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X341, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X341, &
         wf%eri_t1%n_J, &
         X340, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X340)
      call mem%dealloc(X341)
      call mem%alloc(X342, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X342, &
         wf%n_o)
!
      call mem%alloc(X343, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X342, &
         wf%n_o**2, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X343, &
         wf%n_o**2)
!
      call mem%dealloc(X342)
      call mem%alloc(X344, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X343, X344, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X343)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X344, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X344)
      call mem%alloc(X345, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X345, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X346, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X346, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X347, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X345, &
         wf%n_v*wf%n_o, &
         X346, &
         wf%n_v*wf%n_o, &
         zero, &
         X347, &
         wf%n_o**2)
!
      call mem%dealloc(X345)
      call mem%dealloc(X346)
      call mem%alloc(X348, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X347, X348, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X347)
      call mem%alloc(X349, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X349, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X349, &
         wf%eri_t1%n_J, &
         X348, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X348)
      call mem%dealloc(X349)
      call mem%alloc(X350, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X350, &
         wf%n_o)
!
      call mem%alloc(X351, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X350, &
         wf%n_o, &
         zero, &
         X351, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X350)
      call mem%alloc(X352, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X351, X352, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X351)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X352, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X352)
      call mem%alloc(X353, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X353, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X354, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X354, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X355, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X354, &
         wf%n_v**2, &
         X353, &
         wf%n_v**2, &
         zero, &
         X355, &
         wf%n_o**2)
!
      call mem%dealloc(X353)
      call mem%dealloc(X354)
      call mem%alloc(X356, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X355, X356, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X355)
      call mem%alloc(X357, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X356, &
         wf%n_o**2, &
         zero, &
         X357, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X356)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X357, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X357)
      call mem%alloc(X358, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X358, &
         wf%n_o)
!
      call mem%alloc(X359, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X358, 1, &
         zero, &
         X359, 1)
!
      call mem%dealloc(X358)
      call mem%alloc(X360, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X360, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X359, 1, &
         X360, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X359)
      call mem%dealloc(X360)
      call mem%alloc(X361, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X361, 1)
!
      call mem%alloc(X362, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X361, 1, &
         zero, &
         X362, 1)
!
      call mem%dealloc(X361)
      call mem%alloc(X363, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X363, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X363, &
         wf%eri_t1%n_J*wf%n_o, &
         X362, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X362)
      call mem%dealloc(X363)
      call mem%alloc(X364, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X364, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X365, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X364, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X365, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X364)
      call mem%alloc(X366, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X365, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X366, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X365)
      call add_132_to_123(one, X366, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X366)
      call mem%alloc(X367, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X367, 1)
!
      call mem%alloc(X368, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call sort_to_132(LJ_vv, X368, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%alloc(X369, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X368, &
         wf%n_v*wf%eri_t1%n_J, &
         X367, &
         wf%n_v, &
         zero, &
         X369, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X367)
      call mem%dealloc(X368)
      call mem%alloc(X370, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X369, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X370, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X369)
      call add_132_to_123(one, X370, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X370)
      call mem%alloc(X371, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X371, &
         wf%n_o)
!
      call mem%alloc(X372, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X371, &
         wf%n_o**2, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X372, &
         wf%n_o**2)
!
      call mem%dealloc(X371)
      call mem%alloc(X373, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X372, X373, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X372)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X373, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X373)
      call mem%alloc(X374, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X374, 1)
!
      call mem%alloc(X375, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         v_vovo, &
         wf%n_v, &
         X374, &
         wf%n_v, &
         zero, &
         X375, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X374)
      call mem%alloc(X376, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X375, X376, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X375)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X376, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X376)
      call mem%alloc(X377, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         zero, &
         X377, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X378, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X377, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X378, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X377)
      call mem%alloc(X379, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X378, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X379, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X378)
      call add_132_to_123(one, X379, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X379)
      call mem%alloc(X380, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X380, &
         wf%n_o)
!
      call mem%alloc(X381, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X380, &
         wf%n_o**2, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X381, &
         wf%n_o**2)
!
      call mem%dealloc(X380)
      call mem%alloc(X382, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X381, X382, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X381)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X382, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X382)
      call mem%alloc(X383, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X383, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X384, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X384, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X385, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X383, &
         wf%n_v**2, &
         X384, &
         wf%n_v**2, &
         zero, &
         X385, &
         wf%n_o**2)
!
      call mem%dealloc(X383)
      call mem%dealloc(X384)
      call mem%alloc(X386, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rs_vovo, X386, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X387, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X385, &
         wf%n_o**2, &
         X386, &
         wf%n_v**2, &
         zero, &
         X387, &
         wf%n_o**2)
!
      call mem%dealloc(X385)
      call mem%dealloc(X386)
      call mem%alloc(X388, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1324(X387, X388, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X387)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X388, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X388)
      call mem%alloc(X389, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X389, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X390, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X389, &
         wf%n_v*wf%n_o, &
         zero, &
         X390, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X389)
      call mem%alloc(X391, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X390, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X391, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X390)
      call mem%alloc(X392, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X391, X392, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X391)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X392, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X392)
      call mem%alloc(X393, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Ls_vovo, X393, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X394, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X394, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X395, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X393, &
         wf%n_v*wf%n_o, &
         X394, &
         wf%n_v*wf%n_o, &
         zero, &
         X395, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X393)
      call mem%dealloc(X394)
      call mem%alloc(X396, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(t_vovo, X396, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X397, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X395, &
         wf%n_v*wf%n_o, &
         X396, &
         wf%n_v*wf%n_o, &
         zero, &
         X397, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X396)
      call mem%dealloc(X395)
      call mem%alloc(X398, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X397, X398, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X397)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X398, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X398)
      call mem%alloc(X399, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X399, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X400, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X400, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X401, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X400, &
         wf%n_v*wf%n_o, &
         X399, &
         wf%n_v*wf%n_o, &
         zero, &
         X401, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X399)
      call mem%dealloc(X400)
      call mem%alloc(X402, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X401, X402, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X401)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X402, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X402)
      call mem%alloc(X403, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(t_vovo, X403, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X404, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Ls_vovo, X404, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X405, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X403, &
         wf%n_v*wf%n_o, &
         X404, &
         wf%n_v*wf%n_o, &
         zero, &
         X405, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X403)
      call mem%dealloc(X404)
      call mem%alloc(X406, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X406, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X407, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X406, &
         wf%n_v*wf%n_o, &
         X405, &
         wf%n_v*wf%n_o, &
         zero, &
         X407, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X405)
      call mem%dealloc(X406)
      call mem%alloc(X408, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X407, X408, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X407)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X408, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X408)
      call mem%alloc(X409, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X409, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X410, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rs_vovo, X410, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X411, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X409, &
         wf%n_v**2, &
         X410, &
         wf%n_v**2, &
         zero, &
         X411, &
         wf%n_o**2)
!
      call mem%dealloc(X409)
      call mem%dealloc(X410)
      call mem%alloc(X412, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X412, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X413, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X411, &
         wf%n_o**2, &
         X412, &
         wf%n_v**2, &
         zero, &
         X413, &
         wf%n_o**2)
!
      call mem%dealloc(X411)
      call mem%dealloc(X412)
      call mem%alloc(X414, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1324(X413, X414, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X413)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X414, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X414)
      call mem%alloc(X415, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X415, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X416, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X415, &
         wf%n_v*wf%n_o, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X416, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X415)
      call mem%alloc(X417, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X416, X417, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X416)
      call mem%alloc(X418, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X417, &
         wf%n_v*wf%n_o, &
         zero, &
         X418, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X417)
      call add_132_to_123(one, X418, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X418)
      call mem%alloc(X419, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X419, &
         wf%n_v)
!
      call mem%alloc(X420, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X420, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X421, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X420, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X421, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X420)
      call mem%alloc(X422, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X421, X422, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X421)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X422, &
         wf%eri_t1%n_J*wf%n_o, &
         X419, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X419)
      call mem%dealloc(X422)
      call mem%alloc(X423, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X423, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X424, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X423, &
         wf%n_v*wf%n_o, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X424, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X423)
      call mem%alloc(X425, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X425, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X426, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X424, X426, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X424)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X425, &
         wf%eri_t1%n_J, &
         X426, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X425)
      call mem%dealloc(X426)
      call mem%alloc(X427, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X427, &
         wf%n_v)
!
      call mem%alloc(X428, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X427, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X428, &
         wf%n_v)
!
      call mem%dealloc(X427)
      call mem%alloc(X429, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X428, X429, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X428)
      call mem%alloc(X430, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X429, &
         wf%n_v*wf%n_o, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X430, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X429)
      call add_132_to_123(one, X430, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X430)
      call mem%alloc(X431, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X431, &
         wf%n_o)
!
      call mem%alloc(X432, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X432, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X433, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X432, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X433, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X432)
      call mem%alloc(X434, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X433, &
         wf%n_v*wf%eri_t1%n_J, &
         X431, &
         wf%n_o, &
         zero, &
         X434, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X431)
      call mem%dealloc(X433)
      call add_132_to_123(one, X434, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X434)
      call mem%alloc(X435, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X435, &
         wf%n_o)
!
      call mem%alloc(X436, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X436, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X437, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X436, &
         wf%n_v*wf%eri_t1%n_J, &
         X435, &
         wf%n_o, &
         zero, &
         X437, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X435)
      call mem%dealloc(X436)
      call mem%alloc(X438, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X437, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X438, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X437)
      call add_132_to_123(one, X438, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X438)
      X439 = ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rs_vovo, 1)
      call mem%alloc(X440, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X440, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X441, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         X439, &
         X440, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X441, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X440)
      call add_132_to_123(one, X441, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X441)
      call mem%alloc(X442, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X442, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X443, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s_vovo, X443, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X444, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X442, &
         wf%n_v**2, &
         X443, &
         wf%n_v**2, &
         zero, &
         X444, &
         wf%n_o**2)
!
      call mem%dealloc(X442)
      call mem%dealloc(X443)
      call mem%alloc(X445, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X445, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X446, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X444, &
         wf%n_o**2, &
         X445, &
         wf%n_v**2, &
         zero, &
         X446, &
         wf%n_o**2)
!
      call mem%dealloc(X444)
      call mem%dealloc(X445)
      call mem%alloc(X447, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1324(X446, X447, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X446)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X447, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X447)
      call mem%alloc(X448, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X448, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X449, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X449, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X450, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X449, &
         wf%n_v*wf%n_o, &
         X448, &
         wf%n_v*wf%n_o, &
         zero, &
         X450, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X448)
      call mem%dealloc(X449)
      call mem%alloc(X451, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X450, X451, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X450)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X451, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X451)
      call mem%alloc(X452, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Ls_vovo, X452, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X453, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X453, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X454, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X452, &
         wf%n_v*wf%n_o, &
         X453, &
         wf%n_v*wf%n_o, &
         zero, &
         X454, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X452)
      call mem%dealloc(X453)
      call mem%alloc(X455, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(s_vovo, X455, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X456, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X454, &
         wf%n_v*wf%n_o, &
         X455, &
         wf%n_v*wf%n_o, &
         zero, &
         X456, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X454)
      call mem%dealloc(X455)
      call mem%alloc(X457, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X456, X457, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X456)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X457, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X457)
      call mem%alloc(X458, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X458, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X459, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X458, &
         wf%n_v*wf%n_o, &
         zero, &
         X459, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X458)
      call mem%alloc(X460, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X459, &
         wf%n_v*wf%n_o, &
         s_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X460, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X459)
      call mem%alloc(X461, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X460, X461, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X460)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X461, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X461)
      call mem%alloc(X462, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Ls_vovo, X462, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X463, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X463, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X464, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X462, &
         wf%n_v*wf%n_o, &
         X463, &
         wf%n_v*wf%n_o, &
         zero, &
         X464, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X462)
      call mem%dealloc(X463)
      call mem%alloc(X465, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(s_vovo, X465, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X466, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X464, &
         wf%n_v*wf%n_o, &
         X465, &
         wf%n_v*wf%n_o, &
         zero, &
         X466, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X464)
      call mem%dealloc(X465)
      call mem%alloc(X467, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X466, X467, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X466)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X467, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X467)
      call mem%alloc(X468, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X468, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X469, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X469, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X470, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X468, &
         wf%n_v**2, &
         X469, &
         wf%n_v**2, &
         zero, &
         X470, &
         wf%n_o**2)
!
      call mem%dealloc(X468)
      call mem%dealloc(X469)
      call mem%alloc(X471, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s_vovo, X471, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X472, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X470, &
         wf%n_o**2, &
         X471, &
         wf%n_v**2, &
         zero, &
         X472, &
         wf%n_o**2)
!
      call mem%dealloc(X470)
      call mem%dealloc(X471)
      call mem%alloc(X473, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1324(X472, X473, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X472)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X473, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X473)
      call mem%alloc(X474, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X474, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X475, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X474, &
         wf%n_v*wf%n_o, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X475, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X474)
      call mem%alloc(X476, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X475, X476, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X475)
      call mem%alloc(X477, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X476, &
         wf%n_v*wf%n_o, &
         zero, &
         X477, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X476)
      call add_132_to_123(one, X477, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X477)
      call mem%alloc(X478, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X478, &
         wf%n_v)
!
      call mem%alloc(X479, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X479, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X480, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X479, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X480, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X479)
      call mem%alloc(X481, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X480, X481, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X480)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X481, &
         wf%eri_t1%n_J*wf%n_o, &
         X478, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X478)
      call mem%dealloc(X481)
      call mem%alloc(X482, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X482, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X483, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X482, &
         wf%n_v*wf%n_o, &
         zero, &
         X483, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X482)
      call mem%alloc(X484, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X483, X484, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X483)
      call mem%alloc(X485, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X484, &
         wf%n_v*wf%n_o, &
         zero, &
         X485, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X484)
      call add_132_to_123(one, X485, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X485)
      call mem%alloc(X486, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X486, &
         wf%n_v)
!
      call mem%alloc(X487, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X486, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X487, &
         wf%n_v)
!
      call mem%dealloc(X486)
      call mem%alloc(X488, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X487, X488, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X487)
      call mem%alloc(X489, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X488, &
         wf%n_v*wf%n_o, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X489, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X488)
      call add_132_to_123(one, X489, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X489)
      call mem%alloc(X490, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X490, &
         wf%n_o)
!
      call mem%alloc(X491, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X491, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X492, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X491, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X492, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X491)
      call mem%alloc(X493, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X492, &
         wf%n_v*wf%eri_t1%n_J, &
         X490, &
         wf%n_o, &
         zero, &
         X493, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X490)
      call mem%dealloc(X492)
      call add_132_to_123(one, X493, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X493)
      call mem%alloc(X494, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X494, &
         wf%n_o)
!
      call mem%alloc(X495, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X495, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X496, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X495, &
         wf%n_v*wf%eri_t1%n_J, &
         X494, &
         wf%n_o, &
         zero, &
         X496, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X494)
      call mem%dealloc(X495)
      call mem%alloc(X497, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X496, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X497, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X496)
      call add_132_to_123(one, X497, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X497)
      X498 = ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rt_vovo, 1)
      call mem%alloc(X499, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X499, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X500, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         X498, &
         X499, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X500, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X499)
      call add_132_to_123(one, X500, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X500)
      call mem%alloc(X501, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X501, &
         wf%n_o)
!
      call mem%alloc(X502, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X502, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X503, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X502, &
         wf%n_v*wf%eri_t1%n_J, &
         X501, &
         wf%n_o, &
         zero, &
         X503, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X501)
      call mem%dealloc(X502)
      call mem%alloc(X504, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X503, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X504, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X503)
      call add_132_to_123(one, X504, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X504)
      call mem%alloc(X505, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X505, &
         wf%n_v)
!
      call mem%alloc(X506, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X505, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X506, &
         wf%n_v)
!
      call mem%dealloc(X505)
      call mem%alloc(X507, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X506, X507, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X506)
      call mem%alloc(X508, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X507, &
         wf%n_v*wf%n_o, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X508, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X507)
      call add_132_to_123(one, X508, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X508)
      call mem%alloc(X509, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X509, &
         wf%n_o)
!
      call mem%alloc(X510, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X510, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X511, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X510, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X511, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X510)
      call mem%alloc(X512, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X511, &
         wf%n_v*wf%eri_t1%n_J, &
         X509, &
         wf%n_o, &
         zero, &
         X512, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X509)
      call mem%dealloc(X511)
      call add_132_to_123(one, X512, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X512)
      call mem%alloc(X513, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X513, &
         wf%n_v)
!
      call mem%alloc(X514, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X514, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X515, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X514, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X515, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X514)
      call mem%alloc(X516, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X515, X516, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X515)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X516, &
         wf%eri_t1%n_J*wf%n_o, &
         X513, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X513)
      call mem%dealloc(X516)
      call mem%alloc(X517, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X517, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X518, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X517, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X518, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X517)
      call mem%alloc(X519, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X518, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X519, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X518)
      call mem%alloc(X520, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X519, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X520, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X519)
      call add_132_to_123(one, X520, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X520)
      call mem%alloc(X521, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X521, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X522, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X521, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X522, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X521)
      call mem%alloc(X523, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X522, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X523, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X522)
      call mem%alloc(X524, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X523, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X524, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X523)
      call add_132_to_123(one, X524, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X524)
      call mem%alloc(X525, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X525, &
         wf%n_o)
!
      call mem%alloc(X526, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X526, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X527, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X526, &
         wf%n_v*wf%eri_t1%n_J, &
         X525, &
         wf%n_o, &
         zero, &
         X527, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X525)
      call mem%dealloc(X526)
      call mem%alloc(X528, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X527, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X528, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X527)
      call add_132_to_123(one, X528, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X528)
      call mem%alloc(X529, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X529, &
         wf%n_v)
!
      call mem%alloc(X530, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X529, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X530, &
         wf%n_v)
!
      call mem%dealloc(X529)
      call mem%alloc(X531, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X530, X531, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X530)
      call mem%alloc(X532, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X531, &
         wf%n_v*wf%n_o, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X532, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X531)
      call add_132_to_123(one, X532, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X532)
      call mem%alloc(X533, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X533, &
         wf%n_o)
!
      call mem%alloc(X534, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X534, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X535, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X534, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X535, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X534)
      call mem%alloc(X536, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X535, &
         wf%n_v*wf%eri_t1%n_J, &
         X533, &
         wf%n_o, &
         zero, &
         X536, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X533)
      call mem%dealloc(X535)
      call add_132_to_123(one, X536, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X536)
      call mem%alloc(X537, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X537, &
         wf%n_v)
!
      call mem%alloc(X538, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X538, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X539, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X538, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X539, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X538)
      call mem%alloc(X540, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X539, X540, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X539)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X540, &
         wf%eri_t1%n_J*wf%n_o, &
         X537, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X537)
      call mem%dealloc(X540)
      call mem%alloc(X541, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X541, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X542, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X541, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X542, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X541)
      call mem%alloc(X543, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X542, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X543, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X542)
      call mem%alloc(X544, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X543, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X544, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X543)
      call add_132_to_123(one, X544, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X544)
      call mem%alloc(X545, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X545, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X546, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X545, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X546, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X545)
      call mem%alloc(X547, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X546, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X547, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X546)
      call mem%alloc(X548, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X547, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X548, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X547)
      call add_132_to_123(one, X548, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X548)
      call mem%alloc(X549, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X549, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X550, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X549, &
         wf%n_v*wf%n_o, &
         zero, &
         X550, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X549)
      call mem%alloc(X551, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X550, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X551, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X550)
      call mem%alloc(X552, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X551, X552, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X551)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X552, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X552)
      call mem%alloc(X553, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X553, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X554, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X554, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X555, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         Rs, &
         X553, &
         wf%n_v**2, &
         X554, &
         wf%n_v**2, &
         zero, &
         X555, &
         wf%n_o**2)
!
      call mem%dealloc(X553)
      call mem%dealloc(X554)
      call mem%alloc(X556, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X556, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X557, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X555, &
         wf%n_o**2, &
         X556, &
         wf%n_v**2, &
         zero, &
         X557, &
         wf%n_o**2)
!
      call mem%dealloc(X555)
      call mem%dealloc(X556)
      call mem%alloc(X558, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1324(X557, X558, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X557)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X558, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X558)
      call mem%alloc(X559, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Ls_vovo, X559, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X560, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X560, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X561, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X559, &
         wf%n_v*wf%n_o, &
         X560, &
         wf%n_v*wf%n_o, &
         zero, &
         X561, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X559)
      call mem%dealloc(X560)
      call mem%alloc(X562, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(t_vovo, X562, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X563, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X561, &
         wf%n_v*wf%n_o, &
         X562, &
         wf%n_v*wf%n_o, &
         zero, &
         X563, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X561)
      call mem%dealloc(X562)
      call mem%alloc(X564, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X563, X564, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X563)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X564, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X564)
      call mem%alloc(X565, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -Rs, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X565, &
         wf%n_v)
!
      call mem%alloc(X566, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X566, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X567, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X566, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X567, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X566)
      call mem%alloc(X568, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X567, X568, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X567)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X568, &
         wf%eri_t1%n_J*wf%n_o, &
         X565, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X565)
      call mem%dealloc(X568)
      call mem%alloc(X569, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -Rs, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X569, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X570, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X569, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X570, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X569)
      call mem%alloc(X571, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X570, X571, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X570)
      call mem%alloc(X572, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X571, &
         wf%n_v*wf%n_o, &
         zero, &
         X572, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X571)
      call add_132_to_123(one, X572, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X572)
      call mem%alloc(X573, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -Rs, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X573, &
         wf%n_v)
!
      call mem%alloc(X574, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X573, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X574, &
         wf%n_v)
!
      call mem%dealloc(X573)
      call mem%alloc(X575, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X574, X575, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X574)
      call mem%alloc(X576, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X575, &
         wf%n_v*wf%n_o, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X576, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X575)
      call add_132_to_123(one, X576, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X576)
      call mem%alloc(X577, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -Rs, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X577, &
         wf%n_o)
!
      call mem%alloc(X578, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X578, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X579, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X578, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X579, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X578)
      call mem%alloc(X580, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X579, &
         wf%n_v*wf%eri_t1%n_J, &
         X577, &
         wf%n_o, &
         zero, &
         X580, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X577)
      call mem%dealloc(X579)
      call add_132_to_123(one, X580, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X580)
      call mem%alloc(X581, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -Rs, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X581, &
         wf%n_o)
!
      call mem%alloc(X582, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X582, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X583, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X582, &
         wf%n_v*wf%eri_t1%n_J, &
         X581, &
         wf%n_o, &
         zero, &
         X583, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X581)
      call mem%dealloc(X582)
      call mem%alloc(X584, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X583, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X584, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X583)
      call add_132_to_123(one, X584, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X584)
      call mem%alloc(X585, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X585, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X586, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X585, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X586, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X585)
      call mem%alloc(X587, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         X586, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X587, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X586)
      call mem%alloc(X588, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X587, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X588, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X587)
      call add_132_to_123(one, X588, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X588)
      call mem%alloc(X589, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X589, &
         wf%n_o)
!
      call mem%alloc(X590, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X589, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X590, &
         wf%n_o)
!
      call mem%dealloc(X589)
      call mem%alloc(X591, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X590, 1, &
         zero, &
         X591, 1)
!
      call mem%dealloc(X590)
      call mem%alloc(X592, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X592, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X591, 1, &
         X592, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X591)
      call mem%dealloc(X592)
      call mem%alloc(X593, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X593, &
         wf%n_v)
!
      call mem%alloc(X594, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X593, &
         wf%n_v, &
         zero, &
         X594, &
         wf%n_o)
!
      call mem%dealloc(X593)
      call mem%alloc(X595, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X594, 1, &
         zero, &
         X595, 1)
!
      call mem%dealloc(X594)
      call mem%alloc(X596, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X596, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X595, 1, &
         X596, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X595)
      call mem%dealloc(X596)
      call mem%alloc(X597, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X597, &
         wf%n_o)
!
      call mem%alloc(X598, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X597, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X598, &
         wf%n_o)
!
      call mem%dealloc(X597)
      call mem%alloc(X599, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X598, &
         wf%n_o, &
         zero, &
         X599, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X598)
      call mem%alloc(X600, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X599, X600, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X599)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X600, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X600)
      call mem%alloc(X601, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X601, &
         wf%n_v)
!
      call mem%alloc(X602, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X601, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X602, &
         wf%n_v)
!
      call mem%dealloc(X601)
      call mem%alloc(X603, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X602, &
         wf%n_v, &
         zero, &
         X603, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X602)
      call mem%alloc(X604, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X603, X604, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X603)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X604, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X604)
      call mem%alloc(X605, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X605, &
         wf%n_o)
!
      call mem%alloc(X606, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X605, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X606, &
         wf%n_o)
!
      call mem%dealloc(X605)
      call mem%alloc(X607, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X607, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X608, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X607, X608, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X607)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X608, &
         wf%eri_t1%n_J*wf%n_o, &
         X606, &
         wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X606)
      call mem%dealloc(X608)
      call mem%alloc(X609, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X609, &
         wf%n_o)
!
      call mem%alloc(X610, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X609, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X610, &
         wf%n_o)
!
      call mem%dealloc(X609)
      call mem%alloc(X611, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X611, wf%n_v, wf%n_o)
      call mem%alloc(X612, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X611, 1, &
         zero, &
         X612, 1)
!
      call mem%dealloc(X611)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X612, 1, &
         X610, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X610)
      call mem%dealloc(X612)
      call mem%alloc(X613, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X613, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X614, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X614, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X615, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X614, &
         wf%n_v**2, &
         X613, &
         wf%n_v**2, &
         zero, &
         X615, &
         wf%n_o**2)
!
      call mem%dealloc(X613)
      call mem%dealloc(X614)
      call mem%alloc(X616, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X616, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X617, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X615, X617, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X615)
      call mem%alloc(X618, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X616, &
         wf%eri_t1%n_J, &
         X617, &
         wf%n_o**2, &
         zero, &
         X618, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X617)
      call mem%dealloc(X616)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X618, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X618)
      call mem%alloc(X619, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X619, &
         wf%n_o)
!
      call mem%alloc(X620, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X619, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X620, &
         wf%n_o**2)
!
      call mem%dealloc(X619)
      call mem%alloc(X621, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X620, X621, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X620)
      call mem%alloc(X622, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X621, &
         wf%n_v*wf%n_o, &
         zero, &
         X622, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X621)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X622, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X622)
      call mem%alloc(X623, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X623, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X624, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X624, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X625, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X623, &
         wf%n_v*wf%n_o, &
         X624, &
         wf%n_v*wf%n_o, &
         zero, &
         X625, &
         wf%n_o**2)
!
      call mem%dealloc(X623)
      call mem%dealloc(X624)
      call mem%alloc(X626, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X625, X626, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X625)
      call mem%alloc(X627, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X626, &
         wf%n_o**2, &
         zero, &
         X627, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X626)
      call mem%alloc(X628, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X627, X628, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X627)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X628, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X628)
      call mem%alloc(X629, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X629, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X630, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X630, &
         wf%n_o)
!
      call mem%alloc(X631, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X630, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X631, &
         wf%n_o)
!
      call mem%dealloc(X630)
      call mem%alloc(X632, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X629, X632, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X629)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X632, &
         wf%eri_t1%n_J*wf%n_o, &
         X631, &
         wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X632)
      call mem%dealloc(X631)
      call mem%alloc(X633, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X633, &
         wf%n_v)
!
      call mem%alloc(X634, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X633, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X634, &
         wf%n_v)
!
      call mem%dealloc(X633)
      call mem%alloc(X635, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X635, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X636, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X635, X636, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X635)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X636, &
         wf%eri_t1%n_J*wf%n_o, &
         X634, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X634)
      call mem%dealloc(X636)
      call mem%alloc(X637, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X637, &
         wf%n_o)
!
      call mem%alloc(X638, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X637, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X638, &
         wf%n_o)
!
      call mem%dealloc(X637)
      call mem%alloc(X639, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X639, wf%n_v, wf%n_o)
      call mem%alloc(X640, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X639, 1, &
         zero, &
         X640, 1)
!
      call mem%dealloc(X639)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X640, 1, &
         X638, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X638)
      call mem%dealloc(X640)
      call mem%alloc(X641, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X641, &
         wf%n_v)
!
      call mem%alloc(X642, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X641, &
         wf%n_v, &
         zero, &
         X642, &
         wf%n_o)
!
      call mem%dealloc(X641)
      call mem%alloc(X643, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X643, wf%n_v, wf%n_o)
      call mem%alloc(X644, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X643, 1, &
         zero, &
         X644, 1)
!
      call mem%dealloc(X643)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X644, 1, &
         X642, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X642)
      call mem%dealloc(X644)
      call mem%alloc(X645, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X645, &
         wf%n_o)
!
      call mem%alloc(X646, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X645, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X646, &
         wf%n_o)
!
      call mem%dealloc(X645)
      call mem%alloc(X647, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X646, 1, &
         zero, &
         X647, 1)
!
      call mem%dealloc(X646)
      call mem%alloc(X648, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X648, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X647, 1, &
         X648, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X647)
      call mem%dealloc(X648)
      call mem%alloc(X649, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X649, &
         wf%n_o)
!
      call mem%alloc(X650, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X649, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X650, &
         wf%n_o)
!
      call mem%dealloc(X649)
      call mem%alloc(X651, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X650, &
         wf%n_o, &
         zero, &
         X651, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X650)
      call mem%alloc(X652, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X651, X652, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X651)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X652, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X652)
      call mem%alloc(X653, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X653, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X654, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X654, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X655, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X654, &
         wf%n_v**2, &
         X653, &
         wf%n_v**2, &
         zero, &
         X655, &
         wf%n_o**2)
!
      call mem%dealloc(X653)
      call mem%dealloc(X654)
      call mem%alloc(X656, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X656, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X657, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X655, X657, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X655)
      call mem%alloc(X658, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X656, &
         wf%eri_t1%n_J, &
         X657, &
         wf%n_o**2, &
         zero, &
         X658, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X657)
      call mem%dealloc(X656)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X658, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X658)
      call mem%alloc(X659, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X659, &
         wf%n_o)
!
      call mem%alloc(X660, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X659, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X660, &
         wf%n_o**2)
!
      call mem%dealloc(X659)
      call mem%alloc(X661, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X661, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X662, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X660, X662, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X660)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X661, &
         wf%eri_t1%n_J, &
         X662, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X662)
      call mem%dealloc(X661)
      call mem%alloc(X663, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X663, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X664, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X664, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X665, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X663, &
         wf%n_v*wf%n_o, &
         X664, &
         wf%n_v*wf%n_o, &
         zero, &
         X665, &
         wf%n_o**2)
!
      call mem%dealloc(X663)
      call mem%dealloc(X664)
      call mem%alloc(X666, wf%n_o, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X666, &
         wf%n_o)
!
      call mem%alloc(X667, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X665, X667, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X665)
      call mem%alloc(X668, wf%n_o, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X666, X668, wf%n_o, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X666)
!
      call dgemm('T', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X668, &
         wf%n_o**2, &
         X667, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X667)
      call mem%dealloc(X668)
      call mem%alloc(X669, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X669, &
         wf%n_v)
!
      call mem%alloc(X670, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X669, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X670, &
         wf%n_v)
!
      call mem%dealloc(X669)
      call mem%alloc(X671, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X670, &
         wf%n_v, &
         zero, &
         X671, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X670)
      call mem%alloc(X672, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X671, X672, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X671)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X672, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X672)
      call mem%alloc(X673, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X673, &
         wf%n_v)
!
      call mem%alloc(X674, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X673, &
         wf%n_v, &
         zero, &
         X674, &
         wf%n_o)
!
      call mem%dealloc(X673)
      call mem%alloc(X675, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X675, wf%n_v, wf%n_o)
      call mem%alloc(X676, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X675, 1, &
         zero, &
         X676, 1)
!
      call mem%dealloc(X675)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X676, 1, &
         X674, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X674)
      call mem%dealloc(X676)
      call mem%alloc(X677, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X677, &
         wf%n_o)
!
      call mem%alloc(X678, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X677, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X678, &
         wf%n_o**2)
!
      call mem%dealloc(X677)
      call mem%alloc(X679, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X679, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X680, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X678, X680, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X678)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X679, &
         wf%eri_t1%n_J, &
         X680, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X680)
      call mem%dealloc(X679)
      call mem%alloc(X681, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X681, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X682, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X682, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X683, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X681, &
         wf%n_v*wf%n_o, &
         X682, &
         wf%n_v*wf%n_o, &
         zero, &
         X683, &
         wf%n_o**2)
!
      call mem%dealloc(X681)
      call mem%dealloc(X682)
      call mem%alloc(X684, wf%n_o, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X684, &
         wf%n_o)
!
      call mem%alloc(X685, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X683, X685, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X683)
      call mem%alloc(X686, wf%n_o, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X684, X686, wf%n_o, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X684)
!
      call dgemm('T', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X686, &
         wf%n_o**2, &
         X685, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X685)
      call mem%dealloc(X686)
      call mem%alloc(X687, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X687, &
         wf%n_o)
!
      call mem%alloc(X688, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X687, X688, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X687)
      call mem%alloc(X689, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o**3, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X688, &
         wf%n_o**3, &
         zero, &
         X689, &
         wf%n_o)
!
      call mem%dealloc(X688)
      call mem%alloc(X690, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X690, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X691, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X689, &
         wf%n_o**2, &
         X690, &
         wf%n_v**2, &
         zero, &
         X691, &
         wf%n_o**2)
!
      call mem%dealloc(X689)
      call mem%dealloc(X690)
      call mem%alloc(X692, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1324(X691, X692, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X691)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X692, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X692)
      call mem%alloc(X693, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X693, &
         wf%n_v)
!
      call mem%alloc(X694, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X693, &
         wf%n_v, &
         zero, &
         X694, &
         wf%n_o)
!
      call mem%dealloc(X693)
      call mem%alloc(X695, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X694, 1, &
         zero, &
         X695, 1)
!
      call mem%dealloc(X694)
      call mem%alloc(X696, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X696, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X695, 1, &
         X696, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X695)
      call mem%dealloc(X696)
      call mem%alloc(X697, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X697, &
         wf%n_o)
!
      call mem%alloc(X698, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X697, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X698, &
         wf%n_o**2)
!
      call mem%dealloc(X697)
      call mem%alloc(X699, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X698, X699, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X698)
      call mem%alloc(X700, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X699, &
         wf%n_v*wf%n_o, &
         zero, &
         X700, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X699)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X700, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X700)
      call mem%alloc(X701, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X701, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X702, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X702, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X703, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X701, &
         wf%n_v*wf%n_o, &
         X702, &
         wf%n_v*wf%n_o, &
         zero, &
         X703, &
         wf%n_o**2)
!
      call mem%dealloc(X701)
      call mem%dealloc(X702)
      call mem%alloc(X704, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X703, X704, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X703)
      call mem%alloc(X705, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X704, &
         wf%n_o**2, &
         zero, &
         X705, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X704)
      call mem%alloc(X706, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X705, X706, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X705)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X706, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X706)
      call mem%alloc(X707, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X707, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X708, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X708, &
         wf%n_v)
!
      call mem%alloc(X709, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X708, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X709, &
         wf%n_v)
!
      call mem%dealloc(X708)
      call mem%alloc(X710, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X707, X710, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X707)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X710, &
         wf%eri_t1%n_J*wf%n_o, &
         X709, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X710)
      call mem%dealloc(X709)
      call mem%alloc(X711, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X711, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X712, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(X711, X712, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X711)
      call mem%alloc(X713, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X712, &
         wf%n_o**3, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X713, &
         wf%n_o**3)
!
      call mem%dealloc(X712)
      call mem%alloc(X714, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X714, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X715, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X713, &
         wf%n_o**2, &
         X714, &
         wf%n_v**2, &
         zero, &
         X715, &
         wf%n_o**2)
!
      call mem%dealloc(X713)
      call mem%dealloc(X714)
      call mem%alloc(X716, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1324(X715, X716, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call mem%dealloc(X715)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X716, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X716)
      call mem%alloc(X717, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X717, &
         wf%n_o)
!
      call mem%alloc(X718, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X717, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X718, &
         wf%n_o**2)
!
      call mem%dealloc(X717)
      call mem%alloc(X719, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X718, X719, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X718)
      call mem%alloc(X720, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X719, &
         wf%n_o**2, &
         zero, &
         X720, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X719)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X720, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X720)
      call mem%alloc(X721, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X721, &
         wf%n_o)
!
      call mem%alloc(X722, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X721, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X722, &
         wf%n_o**2)
!
      call mem%dealloc(X721)
      call mem%alloc(X723, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X723, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X724, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X722, X724, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X722)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X723, &
         wf%eri_t1%n_J, &
         X724, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X724)
      call mem%dealloc(X723)
      call mem%alloc(X725, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X725, &
         wf%n_o)
!
      call mem%alloc(X726, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X725, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X726, &
         wf%n_o**2)
!
      call mem%dealloc(X725)
      call mem%alloc(X727, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X726, X727, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X726)
      call mem%alloc(X728, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X727, &
         wf%n_o**2, &
         zero, &
         X728, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X727)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X728, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X728)
      call mem%alloc(X729, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X729, &
         wf%n_o)
!
      call mem%alloc(X730, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X729, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X730, &
         wf%n_o**2)
!
      call mem%dealloc(X729)
      call mem%alloc(X731, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X731, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X732, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X730, X732, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X730)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X731, &
         wf%eri_t1%n_J, &
         X732, &
         wf%n_o**2, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X732)
      call mem%dealloc(X731)
      call mem%alloc(X733, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X733, 1)
!
      call mem%alloc(X734, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X733, 1, &
         zero, &
         X734, 1)
!
      call mem%dealloc(X733)
      call mem%alloc(X735, wf%n_o, wf%n_v)
      call sort_to_21(X734, X735, wf%n_v, wf%n_o)
      call mem%dealloc(X734)
      call mem%alloc(X736, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X735, 1, &
         zero, &
         X736, 1)
!
      call mem%dealloc(X735)
      call mem%alloc(X737, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X737, wf%n_v, wf%n_o)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X736, 1, &
         X737, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X736)
      call mem%dealloc(X737)
      call mem%alloc(X738, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X738, 1)
!
      call mem%alloc(X739, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X738, 1, &
         zero, &
         X739, 1)
!
      call mem%dealloc(X738)
      call mem%alloc(X740, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X739, &
         wf%n_v, &
         zero, &
         X740, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X739)
      call mem%alloc(X741, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X740, X741, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X740)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X741, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X741)
      call mem%alloc(X742, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X742, 1)
!
      call mem%alloc(X743, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X742, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X743, &
         wf%n_v)
!
      call mem%dealloc(X742)
      call mem%alloc(X744, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X744, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X745, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X744, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X745, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X744)
      call mem%alloc(X746, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(X745, X746, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X745)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         X746, &
         wf%eri_t1%n_J*wf%n_o, &
         X743, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X743)
      call mem%dealloc(X746)
      call mem%alloc(X747, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X747, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X748, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X748, 1)
!
      call mem%alloc(X749, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X748, 1, &
         zero, &
         X749, 1)
!
      call mem%dealloc(X748)
      call mem%alloc(X750, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X747, X750, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X747)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X750, &
         wf%eri_t1%n_J*wf%n_o, &
         X749, &
         wf%n_v, &
         one, &
         W_J_ov, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X750)
      call mem%dealloc(X749)
      call mem%alloc(X751, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X751, wf%n_v, wf%n_o)
      call mem%alloc(X752, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X751, 1, &
         zero, &
         X752, 1)
!
      call mem%dealloc(X751)
      call mem%alloc(X753, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X753, 1)
!
      call mem%alloc(X754, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X753, 1, &
         zero, &
         X754, 1)
!
      call mem%dealloc(X753)
      call mem%alloc(X755, wf%n_o, wf%n_v)
      call sort_to_21(X754, X755, wf%n_v, wf%n_o)
      call mem%dealloc(X754)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X752, 1, &
         X755, 1, &
         W_J_ov, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X752)
      call mem%dealloc(X755)
      call mem%alloc(X756, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X756, 1)
!
      call mem%alloc(X757, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X756, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X757, &
         wf%n_v)
!
      call mem%dealloc(X756)
      call mem%alloc(X758, wf%n_v, wf%eri_t1%n_J, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         X757, &
         wf%n_v, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X758, &
         wf%n_v)
!
      call mem%dealloc(X757)
      call mem%alloc(X759, wf%n_v, wf%n_o, wf%eri_t1%n_J)
      call sort_to_132(X758, X759, wf%n_v, wf%eri_t1%n_J, wf%n_o)
      call mem%dealloc(X758)
      call mem%alloc(X760, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X759, &
         wf%n_v*wf%n_o, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X760, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X759)
      call add_132_to_123(one, X760, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X760)
      call mem%alloc(X761, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X761, 1)
!
      call mem%alloc(X762, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X761, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X762, &
         wf%n_o)
!
      call mem%dealloc(X761)
      call mem%alloc(X763, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X763, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X764, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X763, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X764, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X763)
      call mem%alloc(X765, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X764, &
         wf%n_v*wf%eri_t1%n_J, &
         X762, &
         wf%n_o, &
         zero, &
         X765, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X762)
      call mem%dealloc(X764)
      call add_132_to_123(one, X765, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X765)
      call mem%alloc(X766, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X766, 1)
!
      call mem%alloc(X767, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X766, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X767, &
         wf%n_o)
!
      call mem%dealloc(X766)
      call mem%alloc(X768, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X768, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X769, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X768, &
         wf%n_v*wf%eri_t1%n_J, &
         X767, &
         wf%n_o, &
         zero, &
         X769, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X767)
      call mem%dealloc(X768)
      call mem%alloc(X770, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X769, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X770, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X769)
      call add_132_to_123(one, X770, W_J_ov, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X770)
!
   end subroutine W_J_ov_terms_qed_ccsd

