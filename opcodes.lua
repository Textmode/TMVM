local _M = {_NAME="opcodes", _TYPE="module"}

local remap = {
	[0x00] = {'NOP'};
	[0x01] = {'INC_1dACC'};
	[0x02] = {'MOV_1iR_2iR'};
	[0x03] = {'MOV_1dA_2dN'};
	[0x04] = {'MOV_1iN_2dA',       'LOAD_1iN_2dA'};
	[0x05] = {'MOV_1dN_2dA',       'LOAD_1dN_2dA'};
	[0x06] = {'ADD_1dR_2dR_3dACC'};
	[0x07] = {'SWP_1dR_2dR'};
	
	[0x08] = {'SWP_1dA_2dB',       'SWP_1dB_2dA'};
	[0x09] = {'ADD_1dA_2dB_3dACC'};
	[0x0a] = {'SHW_1dA'};
	[0x0b] = {'JMP_1dN',           'MOV_1dN_2dIP',      'LJMP_1dSEG_2dN'};
	[0x0c] = {'JNZ_1dRET_2dN',     'MNZ_1dRET_2dN_3dIP'};
	[0x0d] = {'LES_1dA_2dB_3dRET', 'GTE_2dB_1dA_3dRET'};
	[0x0e] = {'MNZ_1dR_2dR_3dN'};
	[0x0f] = {'MOV_1dR_2dR'};
	
	[0x10] = {'NOT_1dR_2dR_1dR'};
	[0x11] = {'AND_1dR_2dR_3dRET'};
	[0x12] = {'OR_1dR_2dR_3dRET'};
	[0x13] = {'XOR_1dR_2dR_3dRET'};
	[0x14] = {'SHL_1dR_2dR_3dRET'};
	[0x15] = {'SHR_1dR_2dR_3dRET'};
	[0x16] = {'SRE_1dR_2dR_3dRET'};
	[0x17] = {'IN_1dR_2dR'};
	
	[0x18] = {'OUT_1dR_2dR'};
	[0x19] = {'MUL_1dR_2dR_3dRET'};
	[0x1a] = {'SUB_1dR_2dR_3dACC'};
	[0x1b] = {'MOD_1dR_2dR_3dRET'};
	[0x1c] = {'MOD_1dR_2dR_3dACC'};
	[0x1d] = {'DIV_1dR_2dR_3dACC'};
	[0x1e] = {'MUL_1dR_2dR_3dACC'};
	[0x1f] = {'DEC_1dACC'};
	
	[0x20] = {'SUB_1dR_2dR_3dRET'};
	[0x21] = {'MOV_1dN_2dB'};
	[0x22] = {'MOV_1iN_2dB'};
	[0x23] = {'MOV_1dB_2dN'};
	[0x24] = {'LMOV_1dR_2dR_3dR_4dR'};
	[0x25] = {'LJMP_1dR_2dR'};
	[0x26] = {'ROL_1dR_2dR_3dRET'};
	[0x27] = {'ROR_1dR_2dR_3dRET'};
	
	[0x28] = {'LOAD_1iR_2dR'};
	[0x29] = {'LOAD_1iN_2dR'};
	[0x2a] = {'STOR_1dR_2iR'};
	[0x2b] = {'STOR_1dR_2iN'};
	
	
	[0xa0] = {'EQL_1dA_2dB_3dRET'};
	[0xa1] = {'SHW_1dR'};
	[0xa2] = {'DIV_1dR_2dR_3dRET'};
	[0xa3] = {'GTR_1dA_2dB_3dRET', 'LTE_2dB_1dA_3dRET'};
	[0xa4] = {'ADD_1dR_2dR_3dRET'};
	[0xa5] = {'SUB_1dR_2dR_3dRET'};
	[0xa6] = {'ADD_1dA_2dB_3dRET'};
	[0xa7] = {'SUB_1dA_2dB_3dACC'};
	
	[0xff] = {'HLT', 'STOP'};
}

local ins
for i=0x00, 0xff do
	ins = remap[i]
	if ins then
		for j=1, #ins do
			if remap[ins[j]] then 
				assert(("dupliate instruction: %s = 0x%02x,0x%02x"):format(ins[j], i, remap[ins[j]]))
			end
			remap[ins[j]] = i
		end
	end
end

_M.map = remap

return _M;

