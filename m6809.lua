-- This is a Lua port of the C++ 6809 emulator 
-- https://github.com/raybellis/usim which is 
-- released under the Mozilla Public License 2.0

-- The public-facing API is:
-- m6809:read(offset) -- must be provided
-- m6809:write(offset,val) -- must be provided
-- m6809:reset()
-- m6809:tick()
-- m6809.IRQ -- set these to false to fire interrupt
-- m6809.FIRQ
-- m6809.NMI
--
-- Make sure to run m6809:reset()
-- before you start ticking.
-- 
-- For multi-CPU systems, you should require a Clone()
-- of this script.

if not bit then bit = bit32 end

-- mode enum
--[[
local immediate = 0
local direct = 1
local indexed = 2
local extended = 3
local inherent = 4
local relative = 5
]]
local immediate = "immediate"
local direct =    "direct"
local indexed =   "indexed"
local extended =  "extended"
local inherent =  "inherent"
local relative =  "relative"

-- bit helper functions
local function btst(x,n)
	return bit.band(x,bit.lshift(1,n)) ~= 0
end
local function bset(x,n)
	return bit.bor(x,bit.lshift(1,n))
end
local function bclr(x,n)
	return bit.band(x,bit.bnot(bit.lshift(1,n)))
end
local function b(x)
	return x and 1 or 0
end
-- bit extend operations
local function extend5(x)
	x = bit.band(x,0xffff)
	if bit.band(x,0x10) ~= 0 then
		return bit.bor(x,0xffe0)
	else
		return x
	end
end
local function extend8(x)
	x = bit.band(x,0xffff)
	if bit.band(x,0x80) ~= 0 then
		return bit.bor(x,0xff00)
	else
		return x
	end
end

local function add8(a,b)
	return bit.band(a + b,0xff)
end
local function add16(a,b)
	return bit.band(a + b,0xffff)
end

function swap(Table, Pos1, Pos2)
	Table[Pos1], Table[Pos2] = Table[Pos2], Table[Pos1]
	return Table
end

local m6809 = {
	trace = false,

	name = "6809", -- change if you want to e.g. maincpu or subcpu for debugging
	mode = 0, -- ??? the identifier "mode" is used for the enum in m6809.h

	u = 0x0000,  -- stack pointers
	s = 0x0000,

	x = 0x0000,  -- index registers
	y = 0x0000,

	dp = 0x00, -- direct page register

	a = 0x00, -- accumulator A
	b = 0x00, -- accumulator B

	-- flags
	cc = {
		c = false, -- Carry
		v = false, -- Overflow
		z = false, -- Zero
		n = false, -- Negative
		i = false, -- IRQ disable
		h = false, -- Half carry
		f = false, -- FIRQ disable
		e = false, -- Entire
	},

	-- internal processor state
	waiting_sync = false,
	waiting_cwai = false,
	nmi_previous = false,
	cycles = 0,

	ir = 0x0000,
	pc = 0x0000,

	-- instruction tracing
	insn_pc = 0x0000,
	insn = "",
	post = 0x00,
	operand = 0x0000,

	-- external signal pins
	-- active low (false) 
	-- i think
	IRQ  = true,
	FIRQ = true,
	NMI  = true,

	regsize = { -- size of each variable in bytes
		u = 2,
		s = 2,
		x = 2,
		y = 2,
		dp = 1,
		a = 1,
		b = 1,
		d = 2,
		ir = 2,
		pc = 2,
		insn_pc = 2,
		post = 1,
		operand = 2,
		all = 1
	},

	comments = nil,
}


-- The instance metatable that handles special registers
-- but preserves method lookup
local m6809_mt = {
	__index = function(self, key)
		-- First check for special registers
		if key == "d" then
			local d = bit.bor(bit.lshift(self.a, 8), self.b)
			--print(string.format("READING D WORKS (D=%04X)",d))
			return d
		elseif key == "all" then
			return self.cc.all
		end

		-- Then try regular method lookup via inheritance
		local val = m6809[key]  -- This looks up methods
		if val ~= nil then
			return val
		end

		-- Finally fall back to raw get
		return rawget(self, key)
	end,

	__newindex = function(self, key, value)
		-- Handle special registers first
		if key == "d" then
			--print(string.format("SETTING D WORKS (D=%04X)",value))
			self.a = bit.rshift(value, 8)
			self.b = bit.band(value, 0xff)
		elseif key == "all" then
			self.cc.all = value
		else
			-- Regular assignment
			rawset(self, key, value)
		end
	end
}

-- Keep the cc metatable as before
local cc_mt = {
	__index = function(self, key)
		if key == "all" then
			local result = 0
			result = bit.bor(result, bit.lshift(self.c and 1 or 0, 0))
			result = bit.bor(result, bit.lshift(self.v and 1 or 0, 1))
			result = bit.bor(result, bit.lshift(self.z and 1 or 0, 2))
			result = bit.bor(result, bit.lshift(self.n and 1 or 0, 3))
			result = bit.bor(result, bit.lshift(self.i and 1 or 0, 4))
			result = bit.bor(result, bit.lshift(self.h and 1 or 0, 5))
			result = bit.bor(result, bit.lshift(self.f and 1 or 0, 6))
			result = bit.bor(result, bit.lshift(self.e and 1 or 0, 7))
			return result
		end
		return rawget(self, key)
	end,
	__newindex = function(self, key, value)
		if key == "all" then
			self.c = bit.band(bit.rshift(value, 0), 1) ~= 0
			self.v = bit.band(bit.rshift(value, 1), 1) ~= 0
			self.z = bit.band(bit.rshift(value, 2), 1) ~= 0
			self.n = bit.band(bit.rshift(value, 3), 1) ~= 0
			self.i = bit.band(bit.rshift(value, 4), 1) ~= 0
			self.h = bit.band(bit.rshift(value, 5), 1) ~= 0
			self.f = bit.band(bit.rshift(value, 6), 1) ~= 0
			self.e = bit.band(bit.rshift(value, 7), 1) ~= 0
		else
			rawset(self, key, value)
		end
	end
}

-- Set up the cc metatable
setmetatable(m6809.cc, cc_mt)

function m6809:new()
	-- Create a new table for this instance
	local instance = {}

	-- Set up inheritance
	setmetatable(instance, self)
	self.__index = self

	-- Apply our special register handling metatable
	-- This will sit "in front of" the inheritance metatable
	setmetatable(instance, m6809_mt)

	return instance
end


m6809.validate = function(self)
	for i,v in pairs(self) do
		if self.regsize[i] then
			local max = 2^(8*self.regsize[i])
			if v > max or v < 0 then
				error( string.format("%s is too big!! (%d>%d)\nPlease tell the developer.", i, v, max) )
				--self[i] = bit.band(v,max-1)
			end
		end
	end

	for i,v in pairs(self.cc) do
		if type(v) ~= "boolean" then
			error(string.format("cc.%s is not a boolean",i))
		end
	end
end


-- Replace these and only these two functions with your own 
-- implementations for reading and writing single bytes

m6809.read = function(self,offset)
	error("You need to supply your own m6809.read function!")
	-- return buffer.readu8(ram,offset) -- example
end

m6809.write = function(self,offset,val)
	error("You need to supply your own m6809.write function!")
	-- buffer.writeu8(ram,offset,val) -- example
end




-- lil inline do_* functions defined in mc6809.h

m6809.do_br = function(self,test)
	self.cycles += 1
	local offset = extend8(self:fetch_operand())
	if not test then return end
	self.pc = bit.band(self.pc+offset, 0xffff)
end
m6809.do_lbr = function(self,test)
	self.cycles += 1
	local offset = self:fetch_word_operand()
	if not test then return end
	self.pc = bit.band(self.pc+offset, 0xffff)
	self.cycles += 1
end

m6809.do_psh = function(self,sp: string,val)
	self[sp] = add16(self[sp],-1)
	self:write(self[sp],val)
end
m6809.do_psh_w = function(self,sp: string,val)
	self:do_psh(sp,bit.band(val,0xff))
	self:do_psh(sp,bit.rshift(val,8))

	self[sp] = bit.band(self[sp],0xffff) -- just in case
end
m6809.do_pul = function(self,sp: string)
	local t = self:read(self[sp])
	self[sp] += 1
	self[sp] = bit.band(self[sp],0xffff)
	return t
end
m6809.do_pul_w = function(self,sp: string)
	local t = bit.lshift(self:read(self[sp]),8)
	self[sp] += 1
	local t = bit.bor(t,self:read(self[sp]))
	self[sp] += 1
	self[sp] = bit.band(self[sp],0xffff)
	return t
end


m6809.fetch = function(self)
	local tmp = self:read(self.pc)
	self.pc = bit.band(self.pc+1, 0xffff)
	return tmp
end

m6809.fetch_word = function(self)
	local tmp = 0
	tmp = bit.lshift(self:fetch(),8)
	tmp = bit.bor(tmp,self:fetch())
	return tmp
end

m6809.read_word = function(self, offset: number)
	local tmp = 0
	tmp = bit.lshift(self:read(offset),8)
	tmp = bit.bor(tmp,self:read(offset+1))
	return tmp
end

m6809.write_word = function(self,offset: number,val)
	self:write(offset,bit.rshift(val,8))
	self:write(offset+1,bit.band(val,0xff))
end

m6809.reset = function(self)
	self.pc = self:read_word(0xfffe)
	self.cycles = 0
	self.dp = 0x00
	self.d = 0x0000
	self.x = 0x0000
	self.y = 0x0000
	self.cc.all = 0x00
	self.cc.i = true -- disable IRQ
	self.cc.f = true -- disable FIRQ
	self.waiting_sync = false
	self.waiting_cwai = false
	self.nmi_previous = false
end

m6809.tick = function(self)
	self.cycles += 1

	local c_nmi = self.NMI
	local c_firq = self.FIRQ
	local c_irq = self.IRQ

	-- check for NMI falling edge
	local nmi_triggered = (not c_nmi) and (self.nmi_previous)
	self.nmi_previous = c_nmi

	--local nmi_triggered = not c_nmi

	if self.waiting_sync then
		--if NMI or IRQ or FIRQ asserts (flags don't matter)
		if nmi_triggered or (not c_firq) or (not c_irq) then
			self.waiting_sync = false;
			self.NMI = true -- hack?
		else
			return
		end
	end

	-- look for external interrupts

	if nmi_triggered then
		print("Doing NMI")
		self:do_nmi()
		self.NMI = true -- hack?
	elseif (not c_firq) and (not self.cc.f) then
		print("Doing FIRQ")
		self:do_firq()
		self.FIRQ = true -- hack?
	elseif (not c_irq)  and (not self.cc.i) then
		print("Doing IRQ")
		self:do_irq()
		self.IRQ = true -- hack?
	elseif self.waiting_cwai then
		return
	end

	--[[
		On the IRQ hacks:
		In Mappy hardware in MAME, the subcpu is what
		seems to reset the IRQ line after an interrupt
		
		but disabling it (setting m_suspend) in debugger
		everything works fine (except sound of course)
		I haven't tried fully disabling/removing the
		subcpu by modifying MAME source yet
		28/09/2024
	]]

	-- if we got here, then CWAI is no longer in effect
	self.waiting_cwai = false;

	-- remember current instruction address
	self.insn_pc = self.pc
	
	local fail = self:execute_instruction(self:fetch_instruction(),self.insn_pc)
	--self:validate() -- Uncomment to validate ranges of internal values

	-- account for the cycle added at the start
	-- this won't be removed if we return in
	-- CWAI or SYNC to prevent lockups loops like 
	-- while (cpu.cycles <= cycles_per_frame) do cpu:tick() end
	if not fail then self.cycles -= 1 end
end

m6809.wordrefreg = function(self,r)
	local switch = {
		[0] = "d",
		[1] = "x",
		[2] = "y",
		[3] = "u",
		[4] = "s",
		[5] = "pc"
	}
	if switch[r] then 
		return switch[r]
	else
		warn("invalid word register selector")
		return 0
	end
end

m6809.byterefreg = function(self,r)
	local switch = {
		[8] = 'a',
		[9] = 'b',
		[10] = 'all',
		[11] = 'dp'
	}
	if switch[r] then 
		return switch[r]
	else
		warn("invalid byte register selector")
		return 0
	end
end

m6809.ix_refreg = function(self,post)
	post = bit.band(bit.rshift(post,5),0x03)
	local switch = {
		[0] = "x",
		[1] = "y",
		[2] = "u",
		[3] = "s",
	}
	if switch[post] then 
		return switch[post]
	end
	warn("invalid register reference")
	return 0
end

m6809._fetch_indexed_operand = function(self)
	if bit.band(self.post, 0x80) == 0x00 then  			-- ,R + 5 bit offset
		self.cycles += 2
		return bit.band(
			self[self:ix_refreg(self.post)] + extend5(bit.band(self.post, 0x1f)),
			0xffff
		)
	end

	local reg = self:ix_refreg(self.post)  -- Get the register string for repeated use

	local post_mask = bit.band(self.post, 0x1f)
	if post_mask == 0x00 then  	                        -- ,R+
		self.cycles += 3
		return self[reg]
	elseif post_mask == 0x01 or post_mask == 0x11 then  -- ,R++
		self.cycles += 4
		return self[reg]
	elseif post_mask == 0x02 then                       -- ,-R
		self.cycles += 3
		return self[reg]
	elseif post_mask == 0x03 or post_mask == 0x13 then  -- ,--R
		self.cycles += 4
		return self[reg]
	elseif post_mask == 0x04 or post_mask == 0x14 then  -- ,R + 0
		self.cycles += 1
		return self[reg]
	elseif post_mask == 0x05 or post_mask == 0x15 then  -- ,R + B
		self.cycles += 2
		return extend8(self.b) + self[reg]
	elseif post_mask == 0x06 or post_mask == 0x16 then  -- ,R + A
		self.cycles += 2
		return extend8(self.a) + self[reg]
	elseif post_mask == 0x08 or post_mask == 0x18 then  -- ,R + 8-bit
		self.cycles += 1
		self.operand = extend8(self:fetch())
		return self[reg] + self.operand
	elseif post_mask == 0x09 or post_mask == 0x19 then  -- ,R + 16-bit
		self.cycles += 3
		self.operand = self:fetch_word()
		return self[reg] + self.operand
	elseif post_mask == 0x0b or post_mask == 0x1b then  -- ,R + D
		self.cycles += 5
		return self.d + self[reg]
	elseif post_mask == 0x0c or post_mask == 0x1c then  -- ,PC + 8-bit
		self.cycles += 1
		self.operand =  extend8(self:fetch())
		return self.pc + self.operand
	elseif post_mask == 0x0d or post_mask == 0x1d then  -- ,PC + 16-bit
		self.cycles += 3
		self.operand = self:fetch_word()
		return self.pc + self.operand
	elseif post_mask == 0x1f then                       -- [,Address]
		self.cycles += 1
		self.operand = self:fetch_word()
		return self.operand
	else
		warn(string.format("invalid indexed addressing postbyte 0x%02x at PC=%04X",self.post,self.pc))
		return 0
	end
end

m6809.fetch_indexed_operand = function(self)
	return bit.band(self:_fetch_indexed_operand(),0xffff)
end


m6809.fetch_operand = function(self)
	if self.mode == immediate then
		self.operand = extend8(self:fetch())
		return bit.band(self.operand,0xff)
	elseif self.mode == relative then
		local r = self:fetch()
		self.operand = bit.band(self.pc + extend8(r),0xffff)
		return r
	else
		return self:read(self:fetch_effective_address())
	end
end

m6809.fetch_word_operand = function(self)
	if self.mode == immediate then
		self.operand = self:fetch_word()
		return self.operand
	elseif self.mode == relative then
		local r = self:fetch_word()
		self.operand = bit.band(self.pc + r,0xffff)
		return r
	else
		return self:read_word(self:fetch_effective_address())
	end
end

m6809.do_postincrement = function(self)
	local p = bit.band(self.post,0x9f)

	if p == 0x80 then
		self[self:ix_refreg(self.post)] = bit.band(self[self:ix_refreg(self.post)] + 1,0xffff)
	elseif p == 0x90 then
		warn("invalid post-increment operation")
	elseif p == 0x81 or p == 0x91 then
		self[self:ix_refreg(self.post)] = bit.band(self[self:ix_refreg(self.post)] + 2,0xffff)
	end
end

m6809.do_predecrement = function(self)
	local p = bit.band(self.post,0x9f)

	if p == 0x82 then
		self[self:ix_refreg(self.post)] = bit.band(self[self:ix_refreg(self.post)] - 1,0xffff)
	elseif p == 0x92 then
		warn("invalid pre-decrement operation")
	elseif p == 0x83 or p == 0x93 then
		self[self:ix_refreg(self.post)] = bit.band(self[self:ix_refreg(self.post)] - 2,0xffff)
	end
end

m6809.fetch_effective_address = function(self)
	if self.mode == extended then
		self.cycles += 1
		self.operand = self:fetch_word()
		return self.operand
	elseif self.mode == direct then
		self.cycles += 1
		self.operand = self:fetch()
		return bit.bor(bit.band(bit.lshift(self.dp, 8), 0xffff), self.operand)
	elseif self.mode == indexed then
		self.post = self:fetch()

		self:do_predecrement()
		local addr = self:fetch_indexed_operand()
		self:do_postincrement()

		-- handle indirect indexed mode
		if btst(self.post, 4) and btst(self.post, 7) then
			self.cycles += 1
			addr = self:read_word(addr)
		end
		--return addr
		return bit.band(addr,0xffff)
	else
		warn("invalid addressing mode")
		return 0
	end
end

m6809.do_nmi = function(self)
	if not self.waiting_cwai then
		self.cc.e = true
		self:help_psh(0xff,"s","u")
	end
	self.cc.f = true
	self.cc.i = true
	self.pc = self:read_word(0xfffc)
end
m6809.do_firq = function(self)
	if not self.waiting_cwai then
		self.cc.e = false
		self:help_psh(0x81,"s","u")
	end
	self.cc.f = true
	self.cc.i = true
	self.pc = self:read_word(0xfff6)
end
m6809.do_irq = function(self)
	if not self.waiting_cwai then
		self.cc.e = true
		self:help_psh(0xff,"s","u")
	end
	self.cc.f = true
	self.cc.i = true
	self.pc = self:read_word(0xfff8)
end

-- generated by feeding execute_instruction into chatgpt.
-- not the best method i know
-- probably has errors (it had one)

-- edit 22/11/2024 regenerated by asking chatgpt to make
-- a script to convert the  switch-case tables from usim
-- and it  turned out exactly  the same so that narrowed
-- it down at least
local opcode_map = {
	abx   = {0x3a},
	adca  = {0x89, 0x99, 0xa9, 0xb9},
	adcb  = {0xc9, 0xd9, 0xe9, 0xf9},
	adda  = {0x8b, 0x9b, 0xab, 0xbb},
	addb  = {0xcb, 0xdb, 0xeb, 0xfb},
	addd  = {0xc3, 0xd3, 0xe3, 0xf3},
	anda  = {0x84, 0x94, 0xa4, 0xb4},
	andb  = {0xc4, 0xd4, 0xe4, 0xf4},
	andcc = {0x1c},
	asra  = {0x47},
	asrb  = {0x57},
	asr   = {0x07, 0x67, 0x77},
	bcc   = {0x24},
	bcs   = {0x25},
	beq   = {0x27},
	bge   = {0x2c},
	bgt   = {0x2e},
	bhi   = {0x22},
	bita  = {0x85, 0x95, 0xa5, 0xb5},
	bitb  = {0xc5, 0xd5, 0xe5, 0xf5},
	ble   = {0x2f},
	bls   = {0x23},
	blt   = {0x2d},
	bmi   = {0x2b},
	bne   = {0x26},
	bpl   = {0x2a},
	bra   = {0x20},
	lbra  = {0x16},
	brn   = {0x21},
	bsr   = {0x8d},
	lbsr  = {0x17},
	bvc   = {0x28},
	bvs   = {0x29},
	clra  = {0x4e, 0x4f},
	clrb  = {0x5e, 0x5f},
	clr   = {0x0f, 0x6f, 0x7f},
	cmpa  = {0x81, 0x91, 0xa1, 0xb1},
	cmpb  = {0xc1, 0xd1, 0xe1, 0xf1},
	cmpd  = {0x1083, 0x1093, 0x10a3, 0x10b3},
	cmps  = {0x118c, 0x119c, 0x11ac, 0x11bc},
	cmpx  = {0x8c, 0x9c, 0xac, 0xbc},
	cmpu  = {0x1183, 0x1193, 0x11a3, 0x11b3},
	cmpy  = {0x108c, 0x109c, 0x10ac, 0x10bc},
	coma  = {0x42, 0x43, 0x1042},
	comb  = {0x52, 0x53},
	com   = {0x03, 0x62, 0x63, 0x73},
	cwai  = {0x3c},
	daa   = {0x19},
	deca  = {0x4a, 0x4b},
	decb  = {0x5a, 0x5b},
	dec   = {0x0a, 0x0b, 0x6a, 0x6b, 0x7a, 0x7b},
	eora  = {0x88, 0x98, 0xa8, 0xb8},
	eorb  = {0xc8, 0xd8, 0xe8, 0xf8},
	exg   = {0x1e},
	inca  = {0x4c},
	incb  = {0x5c},
	inc   = {0x0c, 0x6c, 0x7c},
	jmp   = {0x0e, 0x6e, 0x7e},
	jsr   = {0x9d, 0xad, 0xbd},
	lda   = {0x86, 0x96, 0xa6, 0xb6},
	ldb   = {0xc6, 0xd6, 0xe6, 0xf6},
	ldd   = {0xcc, 0xdc, 0xec, 0xfc},
	lds   = {0x10ce, 0x10de, 0x10ee, 0x10fe},
	ldu   = {0xce, 0xde, 0xee, 0xfe},
	ldx   = {0x8e, 0x9e, 0xae, 0xbe},
	ldy   = {0x108e, 0x109e, 0x10ae, 0x10be},
	leas  = {0x32},
	leau  = {0x33},
	leax  = {0x30},
	leay  = {0x31},
	lsla  = {0x48},
	lslb  = {0x58},
	lsl   = {0x08, 0x68, 0x78},
	lsra  = {0x44, 0x45},
	lsrb  = {0x54, 0x55},
	lsr   = {0x04, 0x05, 0x64, 0x65, 0x74, 0x75},
	mul   = {0x3d},
	nega  = {0x40, 0x41},
	negb  = {0x50, 0x51},
	neg   = {0x00, 0x01, 0x60, 0x61, 0x70, 0x71},
	nop   = {0x12},
	ora   = {0x8a, 0x9a, 0xaa, 0xba},
	orb   = {0xca, 0xda, 0xea, 0xfa},
	orcc  = {0x1a},
	pshs  = {0x34},
	pshu  = {0x36},
	puls  = {0x35},
	pulu  = {0x37},
	rola  = {0x49},
	rolb  = {0x59},
	rol   = {0x09, 0x69, 0x79},
	rora  = {0x46},
	rorb  = {0x56},
	ror   = {0x06, 0x66, 0x76},
	rti   = {0x3b},
	rts   = {0x39},
	sbca  = {0x82, 0x92, 0xa2, 0xb2},
	sbcb  = {0xc2, 0xd2, 0xe2, 0xf2},
	sex   = {0x1d},
	sta   = {0x97, 0xa7, 0xb7},
	stb   = {0xd7, 0xe7, 0xf7},
	std   = {0xdd, 0xed, 0xfd},
	sts   = {0x10df, 0x10ef, 0x10ff},
	stu   = {0xdf, 0xef, 0xff},
	stx   = {0x9f, 0xaf, 0xbf},
	sty   = {0x109f, 0x10af, 0x10bf},
	suba  = {0x80, 0x90, 0xa0, 0xb0},
	subb  = {0xc0, 0xd0, 0xe0, 0xf0},
	subd  = {0x83, 0x93, 0xa3, 0xb3},
	swi   = {0x3f},
	swi2  = {0x103f},
	swi3  = {0x113f},
	sync  = {0x13},
	tfr   = {0x1f},
	tsta  = {0x4d},
	tstb  = {0x5d},
	tst   = {0x0d, 0x6d, 0x7d},
	lbcc  = {0x1024},
	lbcs  = {0x1025},
	lbeq  = {0x1027},
	lbge  = {0x102c},
	lbgt  = {0x102e},
	lbhi  = {0x1022},
	lble  = {0x102f},
	lbls  = {0x1023},
	lblt  = {0x102d},
	lbmi  = {0x102b},
	lbne  = {0x1026},
	lbpl  = {0x102a},
	lbrn  = {0x1021},
	lbvc  = {0x1028},
	lbvs  = {0x1029},
}

-- Inverted opcode map where each opcode maps directly to a method name
local opcode_to_method = {}

-- Helper function to add opcodes to the new map
local function map_opcodes(method, opcodes)
	for _, opcode in ipairs(opcodes) do
		opcode_to_method[opcode] = method
	end
end

-- Rebuild the opcode map in reverse (opcode -> method)
local num_opcodes = 0
for method, opcodes in pairs(opcode_map) do
	map_opcodes(method, opcodes)
	num_opcodes += 1
end

-- cache addressing modes
local modes = {}

local decode_instruction = function(ir)
	local ir_upper = bit.band(ir, 0xf0) -- Mask the upper 4 bits
	if ir_upper == 0x00 or ir_upper == 0x90 or ir_upper == 0xd0 then
		return direct
	elseif ir_upper == 0x20 then
		return relative
	elseif ir_upper == 0x30 or ir_upper == 0x40 or ir_upper == 0x50 then
		if ir < 0x34 then
			return indexed
		elseif ir < 0x38 or ir == 0x3c then
			return immediate
		else
			return inherent
		end
	elseif ir_upper == 0x60 or ir_upper == 0xa0 or ir_upper == 0xe0 then
		return indexed
	elseif ir_upper == 0x70 or ir_upper == 0xb0 or ir_upper == 0xf0 then
		return extended
	elseif ir_upper == 0x80 or ir_upper == 0xc0 then
		if ir == 0x8d then
			return relative
		else
			return immediate
		end
	elseif ir_upper == 0x10 then
		local ir_lower = bit.band(ir, 0x0f) -- Mask the lower 4 bits
		if ir_lower == 0x02 or ir_lower == 0x03 or ir_lower == 0x09 or ir_lower == 0x0d then
			return inherent
		elseif ir_lower == 0x06 or ir_lower == 0x07 then
			return relative
		elseif ir_lower == 0x0a or ir_lower == 0x0c or ir_lower == 0x0e or ir_lower == 0x0f then
			return immediate
		end
	end
end

for i,v in pairs(opcode_to_method) do
	modes[i] = decode_instruction(i)
end
print(modes)

m6809.fetch_instruction = function(self)
	-- Fetch the instruction register value
	local ir = self:fetch()

	-- Check for two-byte instructions (0x10 or 0x11)
	if ir == 0x10 or ir == 0x11 then
		ir = bit.lshift(ir, 8) -- Shift left 8 bits
		ir = bit.bor(ir, self:fetch()) -- OR it with the next fetched byte
	end
  
	self.mode = modes[ir]
	return ir
end

function m6809:getcomment(addr)
	if self.comments == nil then return "" end
	if self.comments[addr] == nil then return "" else
		return " ; "..self.comments[addr]
	end
end

local function printCC(v)
	local flags = {
		(bit32.band(v, 0x80) ~= 0) and "E" or ".",
		(bit32.band(v, 0x40) ~= 0) and "F" or ".",
		(bit32.band(v, 0x20) ~= 0) and "H" or ".",
		(bit32.band(v, 0x10) ~= 0) and "I" or ".",
		(bit32.band(v, 0x08) ~= 0) and "N" or ".",
		(bit32.band(v, 0x04) ~= 0) and "Z" or ".",
		(bit32.band(v, 0x02) ~= 0) and "V" or ".",
		(bit32.band(v, 0x01) ~= 0) and "C" or "."
	}
	return table.concat(flags)
end

-- Execution function with direct opcode lookup
m6809.execute_instruction = function(self, ir, pc)
	local method = opcode_to_method[ir]

	if method then
		if self[method] == nil then
			warn(self.name .. ": unimplemented instruction: " .. string.upper(method) .. string.format(" (0x%02x, %s) at PC %04x", ir, self.mode, pc))
			return true
		else
			if self.trace then 
				print(string.format("%s: running %s (0x%02x, %s) at PC %04x%s",self.name,string.upper(method),ir,self.mode,pc,self:getcomment(pc)))
				print(string.format("A=%02X   B=%02X   DP=%02X  CC=%02X  %s",self.a,self.b,self.dp,self.cc.all,printCC(self.cc.all)))
				print(string.format("D=%04X X=%04X Y=%04X S=%04X U=%04X",self.d,self.x,self.y,self.s,self.u))
			end
			self[method](self)
			return false
		end
	else
		warn(string.format(self.name .. ": illegal instruction at %04x (0x%02x)", pc, ir))
		return true
	end
end

-- instruction helper functions

function m6809:help_adc(x)
	local m = self:fetch_operand()
	local cc = self.cc

	local t = bit.band(x, 0x0f) + bit.band(m, 0x0f) + (cc.c and 1 or 0)
	t = bit.band(t,0xff)
	cc.h = btst(t,4) -- Half carry

	local t = bit.band(x, 0x7f) + bit.band(m, 0x7f) + (cc.c and 1 or 0)
	t = bit.band(t,0xff)
	cc.v = btst(t,7) -- Bit 7 carry in

	local t = x + m + (cc.c and 1 or 0)
	cc.c = btst(t,8) -- Bit 7 carry out
	x = bit.band(t, 0xff)

	cc.v = cc.v ~= cc.c
	cc.n = btst(x,7)
	cc.z = x == 0

	return x
end

function m6809:help_add(x)
	local m = self:fetch_operand()
	local cc = self.cc

	local t = bit.band(x, 0x0f) + bit.band(m, 0x0f)
	t = bit.band(t,0xff)
	cc.h = btst(t,4) -- Half carry

	local t = bit.band(x, 0x7f) + bit.band(m, 0x7f)
	t = bit.band(t,0xff)
	cc.v = btst(t,7) -- Bit 7 carry in

	local t = x + m
	cc.c = btst(t,8) -- Bit 7 carry out
	x = bit.band(t, 0xff) 

	cc.v = cc.v ~= cc.c
	cc.n = btst(x,7)
	cc.z = x == 0

	return x
end

function m6809:help_and(x)
	x = bit.band(x, self:fetch_operand())

	self.cc.n = btst(x,7)
	self.cc.z = x == 0
	self.cc.v = false

	return x
end

function m6809:help_asr(x)
	self.cc.c = btst(x, 0)
	x = bit.rshift(x, 1)

	-- If the sign bit was set, propagate it
	self.cc.n = btst(x, 6)
	if self.cc.n then
		x = bset(x,7)
	end

	self.cc.z = x == 0
	self.cycles += 1

	return x
end

function m6809:help_bit(x)
	local t = bit.band(x, self:fetch_operand())

	local cc = self.cc
	cc.n = btst(t,7)
	cc.v = false
	cc.z = t == 0
end

function m6809:help_clr(x)
	self.cc.all = bit.band(self.cc.all,0xf0)
	self.cc.all = bit.bor(self.cc.all,0x04)
	self.cycles += 1

	return 0 -- The cleared value is always 0
end

function m6809:help_cmp(x)
	local m = self:fetch_operand()
	local t = x - m

	self.cc.v = btst(bit.bxor(x, m, t, bit.rshift(t,1) ),  7)
	self.cc.c = btst(t,8)
	self.cc.n = btst(t,7)
	self.cc.z = bit.band(t, 0xff) == 0
end

function m6809:help_cmp_w(x)
	local m = self:fetch_word_operand()
	local t = x - m

	self.cc.v = btst(bit.bxor(x, m, t, bit.rshift(t,1) ), 15)
	self.cc.c = btst(t,16)
	self.cc.n = btst(t,15)
	self.cc.z = bit.band(t, 0xffff) == 0

	self.cycles += 1
end

function m6809:help_com(x)
	x = bit.band(bit.bnot(x),0xff)
	self.cc.c = true
	self.cc.v = false
	self.cc.n = btst(x,7)
	self.cc.z = x == 0

	self.cycles += 1

	return x
end

function m6809:help_dec(x)
	self.cc.v = (x == 0x80)
	x = bit.band(x - 1, 0xff)
	self.cc.n = btst(x,7)
	self.cc.z = x == 0
	self.cycles += 1

	return x
end

function m6809:help_eor(x)
	x = bit.bxor(x, self:fetch_operand())

	self.cc.v = false
	self.cc.n = btst(x,7)
	self.cc.z = x == 0

	return x
end

function m6809:help_inc(x)
	self.cc.v = (x == 0x7f)
	x = bit.band(x + 1, 0xff)
	self.cc.n = btst(x,7)
	self.cc.z = x == 0
	self.cycles += 1

	return x
end

function m6809:help_ld()
	local x = self:fetch_operand()

	self.cc.n = btst(x,7)
	self.cc.v = false
	self.cc.z = x == 0

	return x
end

function m6809:help_ld_w()
	local x = self:fetch_word_operand()

	self.cc.n = btst(x,15)
	self.cc.v = false
	self.cc.z = x == 0

	return x
end

function m6809:help_lsl(x)
	self.cc.c = btst(x,7)
	self.cc.v = btst(x,7) ~= btst(x,6)
	x = bit.band(bit.lshift(x, 1), 0xff)
	self.cc.n = btst(x,7)
	self.cc.z = x == 0
	self.cycles += 1

	return x
end

function m6809:help_lsr(x)
	self.cc.c = btst(x,0)
	x = bit.rshift(x, 1)
	self.cc.n = false
	self.cc.z = x == 0
	self.cycles += 1

	return x
end

function m6809:help_neg(x)
	local t = 0 - x

	self.cc.v = btst(bit.bxor(x,t,bit.rshift(t,1)),7)
	self.cc.c = btst(t,8)
	self.cc.n = btst(t,7)
	x = bit.band(t, 0xff)
	self.cc.z = x == 0
	self.cycles += 1

	return x
end

function m6809:help_or(x)
	x = bit.bor(x, self:fetch_operand())

	self.cc.v = false
	self.cc.n = btst(x,7)
	self.cc.z = x == 0

	return x
end

function m6809:help_psh(w,s: string,u: string)
	if (btst(w, 7)) then self:do_psh_w(s, self.pc) end
	if (btst(w, 6)) then self:do_psh_w(s, self[u]) end
	if (btst(w, 5)) then self:do_psh_w(s, self.y) end
	if (btst(w, 4)) then self:do_psh_w(s, self.x) end
	if (btst(w, 3)) then self:do_psh(s, self.dp) end
	if (btst(w, 2)) then self:do_psh(s, self.b) end
	if (btst(w, 1)) then self:do_psh(s, self.a) end
	if (btst(w, 0)) then self:do_psh(s, self.cc.all) end
end

function m6809:help_pul(w,s: string,u: string)
	if (btst(w, 0)) then self.cc.all = self:do_pul(s) end
	if (btst(w, 1)) then self.a = self:do_pul(s) end
	if (btst(w, 2)) then self.b = self:do_pul(s) end
	if (btst(w, 3)) then self.dp = self:do_pul(s) end
	if (btst(w, 4)) then self.x = self:do_pul_w(s) end
	if (btst(w, 5)) then self.y = self:do_pul_w(s) end
	if (btst(w, 6)) then self[u] = self:do_pul_w(s) end
	if (btst(w, 7)) then self.pc = self:do_pul_w(s) end
end


function m6809:help_rol(x)	
	local oc = self.cc.c
	self.cc.v = btst(x, 7) ~= btst(x, 6)
	self.cc.c = btst(x, 7)
	x = bit.band(bit.lshift(x, 1), 0xff)
	if oc then x = bset(x, 0) end
	self.cc.n = btst(x, 7)
	self.cc.z = x == 0
	self.cycles += 1
	return x
end

function m6809:help_ror(x)
	local oc = self.cc.c
	self.cc.c = btst(x, 0)
	x = bit.rshift(x, 1)
	if oc then x = bset(x, 7) end
	self.cc.n = btst(x, 7)
	self.cc.z = x == 0
	self.cycles += 1
	return x
end

function m6809:help_sbc(x)
	local m = self:fetch_operand()
	local t = x - m - (self.cc.c and 1 or 0)
	self.cc.v = btst(bit.bxor(x, m, t, bit.rshift(t, 1)), 7)
	self.cc.c = btst(t, 8)
	self.cc.n = btst(t, 7)
	x = bit.band(t, 0xff)
	self.cc.z = x == 0
	return x
end

function m6809:help_st(x)
	local addr = self:fetch_effective_address()
	self:write(addr, x)
	self.cc.v = false
	self.cc.n = btst(x, 7)
	self.cc.z = x == 0
end

function m6809:help_st_w(x)
	local addr = self:fetch_effective_address()
	self:write_word(addr, x)
	self.cc.v = false
	self.cc.n = btst(x, 15)
	self.cc.z = x == 0
end

function m6809:help_sub(x)
	local m = self:fetch_operand()
	local t = x - m
	self.cc.v = btst(bit.bxor(x, m, t, bit.rshift(t, 1)), 7)
	self.cc.c = btst(t, 8)
	self.cc.n = btst(t, 7)
	x = bit.band(t, 0xff)
	self.cc.z = x == 0
	return x
end

function m6809:help_tst(x)
	self.cc.v = false
	self.cc.n = btst(x, 7)
	self.cc.z = x == 0
	self.cycles += 1
end

-- instructions start here

m6809.abx = function(self)
	self.x = bit.band(self.x + self.b,0xffff)
end
m6809.adca = function(self)
	self.a = self:help_adc(self.a)
end
m6809.adcb = function(self)
	self.b = self:help_adc(self.b)
end
m6809.adda = function(self)
	self.a = self:help_add(self.a)
end
m6809.addb = function(self)
	self.b = self:help_add(self.b)
end
m6809.addd = function(self)
	local m = self:fetch_word_operand()

	local t = bit.band(self.d,0x7fff) + bit.band(m,0x7fff)
	self.cc.v = btst(t,15)

	local t = self.d + m
	self.cc.c = btst(t,16)
	self.d = bit.band(t,0xffff)

	self.cc.v = self.cc.v ~= self.cc.c
	self.cc.n = btst(self.d,15)
	self.cc.z = self.d == 0
end
m6809.anda = function(self)
	self.a = self:help_and(self.a)
end
m6809.andb = function(self)
	self.b = self:help_and(self.b)
end
m6809.andcc = function(self)
	self.cc.all = bit.band(self.cc.all,self:fetch_operand())
	self.cycles += 1
end
m6809.asra = function(self)
	self.a = self:help_asr(self.a)
end
m6809.asrb = function(self)
	self.b = self:help_asr(self.b)
end
m6809.asr = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_asr(m)
	self:write(addr,m)
end

-- branching instructions

m6809.bcc = function(self)
	self:do_br(not self.cc.c)
end
m6809.lbcc = function(self)
	self:do_lbr(not self.cc.c)
end
m6809.bcs = function(self)
	self:do_br(self.cc.c)
end
m6809.lbcs = function(self)
	self:do_lbr(self.cc.c)
end
m6809.beq = function(self)
	self:do_br(self.cc.z)
end
m6809.lbeq = function(self)
	self:do_lbr(self.cc.z)
end
m6809.bge = function(self)
	self:do_br(self.cc.n == self.cc.v)
end
m6809.lbge = function(self)
	self:do_lbr(self.cc.n == self.cc.v)
end
m6809.bgt = function(self)
	self:do_br(not (self.cc.z or (self.cc.n ~= self.cc.v)))
end
m6809.lbgt = function(self)
	self:do_lbr(not (self.cc.z or (self.cc.n ~= self.cc.v)))
end
m6809.bhi = function(self)
	self:do_br(not (self.cc.c or self.cc.z))
end
m6809.lbhi = function(self)
	self:do_lbr(not (self.cc.c or self.cc.z))
end

m6809.bita = function(self)
	self:help_bit(self.a)
end
m6809.bitb = function(self)
	self:help_bit(self.b)
end

m6809.ble = function(self)
	self:do_br(self.cc.z or (self.cc.n ~= self.cc.v))
end
m6809.lble = function(self)
	self:do_lbr(self.cc.z or (self.cc.n ~= self.cc.v))
end
m6809.bls = function(self)
	self:do_br(self.cc.c or self.cc.z)
end
m6809.lbls = function(self)
	self:do_lbr(self.cc.c or self.cc.z)
end
m6809.blt = function(self)
	self:do_br(self.cc.n ~= self.cc.v)
end
m6809.lblt = function(self)
	self:do_lbr(self.cc.n ~= self.cc.v)
end
m6809.bmi = function(self)
	self:do_br(self.cc.n)
end
m6809.lbmi = function(self)
	self:do_lbr(self.cc.n)
end
m6809.bne = function(self)
	self:do_br(not self.cc.z)
end
m6809.lbne = function(self)
	self:do_lbr(not self.cc.z)
end
m6809.bpl = function(self)
	self:do_br(not self.cc.n)
end
m6809.lbpl = function(self)
	self:do_lbr(not self.cc.n)
end
m6809.bra = function(self)
	self:do_br(true)  
end
m6809.lbra = function(self)
	self:do_lbr(true)  
end
m6809.brn = function(self)
	self:do_br(false)  
end
m6809.lbrn = function(self)
	self:do_lbr(false)
end
m6809.bsr = function(self)
	local x = self:fetch_operand()
	self:do_psh_w("s",self.pc)
	self.pc = add16(self.pc,extend8(x))
	self.cycles += 3
end
m6809.lbsr = function(self)
	local x = self:fetch_word_operand()
	self:do_psh_w("s",self.pc)
	self.pc = add16(self.pc,x)
	self.cycles += 4
end
m6809.bvc = function(self)
	self:do_br(not self.cc.v)
end
m6809.lbvc = function(self)
	self:do_lbr(not self.cc.v)
end
m6809.bvs = function(self)
	self:do_br(self.cc.v)
end
m6809.lbvs = function(self)
	self:do_lbr(self.cc.v)
end

-- end of branching instructions

m6809.clra = function(self)
	self.a = self:help_clr()
end
m6809.clrb = function(self)
	self.b = self:help_clr()
end
m6809.clr = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	self:help_clr()
	self:write(addr,0)
end

m6809.cmpa = function(self)
	self:help_cmp(self.a)
end
m6809.cmpb = function(self)
	self:help_cmp(self.b)
end
m6809.cmpd = function(self)
	self:help_cmp_w(self.d)
end
m6809.cmpx = function(self)
	self:help_cmp_w(self.x)
end
m6809.cmpy = function(self)
	self:help_cmp_w(self.y)
end
m6809.cmpu = function(self)
	self:help_cmp_w(self.u)
end
m6809.cmps = function(self)
	self:help_cmp_w(self.s)
end

m6809.cwai = function(self)
	local n = self:fetch_operand()
	self.cc.all = bit.band(self.cc.all,n)
	self.cc.e = 1
	self:help_psh(0xff,'s','u')
	self.cycles += 2
	self.waiting_cwai = true
end

m6809.coma = function(self)
	self.a = self:help_com(self.a)
end
m6809.comb = function(self)
	self.b = self:help_com(self.b)
end
m6809.com = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_com(m)
	self:write(addr,m)
end

m6809.daa = function(self)
	-- not complete (does not touch overflow flags)
	local c = 0
	local lsn = bit.band(self.a, 0x0f)
	local msn = bit.rshift(bit.band(self.a, 0xf0), 4)

	if self.cc.h or (lsn > 9) then
		c = bit.bor(c, 0x06)
	end

	if ( self.cc.c or (msn > 9) ) or ( (msn > 8) and (lsn > 9) ) then
		c = bit.bor(c, 0x60)
	end

	local t = self.a + c
	self.cc.c = self.cc.c or btst(t, 8)
	self.a = bit.band(t, 0xff)

	self.cc.n = btst(self.a, 7)
	self.cc.z = self.a == 0
	self.cycles += 1
end

m6809.deca = function(self)
	self.a = self:help_dec(self.a)
end
m6809.decb = function(self)
	self.b = self:help_dec(self.b)
end
m6809.dec = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_dec(m)
	self:write(addr,m)
end

m6809.eora = function(self)
	self.a = self:help_eor(self.a)
end
m6809.eorb = function(self)
	self.b = self:help_eor(self.b)
end

m6809.inca = function(self)
	self.a = self:help_inc(self.a)
end
m6809.incb = function(self)
	self.b = self:help_inc(self.b)
end
m6809.inc = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_inc(m)
	self:write(addr,m)
end

m6809.jmp = function(self)
	self.pc = self:fetch_effective_address()
end
m6809.jsr = function(self)
	local addr = self:fetch_effective_address()
	self:do_psh_w('s',self.pc)
	self.pc = addr
	self.cycles += 2
end

m6809.lda = function(self)
	self.a = self:help_ld(self.a)
end
m6809.ldb = function(self)
	self.b = self:help_ld(self.b)
end
m6809.ldd = function(self)
	self.d = self:help_ld_w(self.d)
end
m6809.ldx = function(self)
	self.x = self:help_ld_w(self.x)
end
m6809.ldy = function(self)
	self.y = self:help_ld_w(self.y)
end
m6809.lds = function(self)
	self.s = self:help_ld_w(self.s)
end
m6809.ldu = function(self)
	self.u = self:help_ld_w(self.u)
end

m6809.leax = function(self)
	self.x = self:fetch_effective_address()
	self.cc.z = self.x == 0
	self.cycles += 1
end
m6809.leay = function(self)
	self.y = self:fetch_effective_address()
	self.cc.z = self.y == 0
	self.cycles += 1
end
m6809.leas = function(self)
	self.s = self:fetch_effective_address()
	self.cycles += 1
end
m6809.leau = function(self)
	self.u = self:fetch_effective_address()
	self.cycles += 1
end

m6809.lsla = function(self)
	self.a = self:help_lsl(self.a)
end
m6809.lslb = function(self)
	self.b = self:help_lsl(self.b)
end
m6809.lsl = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_lsl(m)
	self:write(addr,m)
end

m6809.lsra = function(self)
	self.a = self:help_lsr(self.a)
end
m6809.lsrb = function(self)
	self.b = self:help_lsr(self.b)
end
m6809.lsr = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_lsr(m)
	self:write(addr,m)
end

m6809.mul = function(self)
	self.d = bit.band(self.a * self.b,0xffff)
	self.cc.c = btst(self.b,7)
	self.cc.z = self.d == 0
	self.cycles += 10
end

m6809.nega = function(self)
	self.a = self:help_neg(self.a)
end
m6809.negb = function(self)
	self.b = self:help_neg(self.b)
end
m6809.neg = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_neg(m)
	self:write(addr,m)
end

m6809.nop = function(self)
	self.cycles += 1
end

m6809.ora = function(self)
	self.a = self:help_or(self.a)
end
m6809.orb = function(self)
	self.b = self:help_or(self.b)
end
m6809.orcc = function(self)
	self.cc.all = bit.bor(self.cc.all,self:fetch_operand())
	self.cycles += 1
end

m6809.pshs = function(self)
	local w = self:fetch_operand()
	self:help_psh(w,'s','u')
	self.cycles += 3
end
m6809.pshu = function(self)
	local w = self:fetch_operand()
	self:help_psh(w,'u','s')
	self.cycles += 3
end
m6809.puls = function(self)
	local w = self:fetch_operand()
	self:help_pul(w,'s','u')
	self.cycles += 3
end
m6809.pulu = function(self)
	local w = self:fetch_operand()
	self:help_pul(w,'u','s')
	self.cycles += 3
end

m6809.rola = function(self)
	self.a = self:help_rol(self.a)
end
m6809.rolb = function(self)
	self.b = self:help_rol(self.b)
end
m6809.rol = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_rol(m)
	self:write(addr,m)
end
m6809.rora = function(self)
	self.a = self:help_ror(self.a)
end
m6809.rorb = function(self)
	self.b = self:help_ror(self.b)
end
m6809.ror = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	m = self:help_ror(m)
	self:write(addr,m)
end

m6809.rti = function(self)
	self:help_pul(0x01,'s','u')
	if (self.cc.e) then
		self:help_pul(0xfe,'s','u')
	else
		self:help_pul(0x80,'s','u')
	end
	self.cycles += 2
end
m6809.rts = function(self)
	self.pc = self:do_pul_w('s')
	self.cycles += 2
end

m6809.sbca = function(self)
	self.a = self:help_sbc(self.a)
end
m6809.sbcb = function(self)
	self.b = self:help_sbc(self.b)
end

m6809.sex = function(self) -- It's "sign extend". Good one Motorola...
	self.cc.n = btst(self.b,7)
	self.cc.z = self.b == 0
	self.a = self.cc.n and 255 or 0
	self.cycles += 1
end

m6809.sta = function(self)
	self:help_st(self.a)
end
m6809.stb = function(self)
	self:help_st(self.b)
end
m6809.std = function(self)
	self:help_st_w(self.d)
end
m6809.stx = function(self)
	self:help_st_w(self.x)
end
m6809.sty = function(self)
	self:help_st_w(self.y)
end
m6809.sts = function(self)
	self:help_st_w(self.s)
end
m6809.stu = function(self)
	self:help_st_w(self.u)
end

m6809.suba = function(self)
	self.a = self:help_sub(self.a)
end
m6809.subb = function(self)
	self.b = self:help_sub(self.b)
end
m6809.subd = function(self)
	local m = self:fetch_word_operand()
	local t = self.d - m

	self.cc.v = btst(bit.bxor(self.d, m, t, bit.rshift(t, 1)), 15)
	self.cc.c = btst(t, 16)
	self.cc.n = btst(t, 15)
  
	self.d = bit.band(t, 0xffff)
	self.cc.z = self.d == 0
end

m6809.swi = function(self)
	self.cc.e = true
	self:help_psh(0xff,'s','u')
	self.cc.f = true
	self.cc.i = true
	self.pc = self:read_word(0xfffa)
	self.cycles += 4
end
m6809.swi2 = function(self)
	self.cc.e = true
	self:help_psh(0xff,'s','u')
	self.pc = self:read_word(0xfff4)
	self.cycles += 4
end
m6809.swi3 = function(self)
	self.cc.e = true
	self:help_psh(0xff,'s','u')
	self.pc = self:read_word(0xfff2)
	self.cycles += 4
end

m6809.sync = function(self)
	self.waiting_sync = true
	self.cycles += 1
end

--[[ EXG & TFR code lifted from MAME ]]--
function m6809:read_tfr_exg_816_register(reg)
	local result = 0xffff

	local switch = {
		[0] = "d",
		[1] = "x",
		[2] = "y",
		[3] = "u",
		[4] = "s",
		[5] = "pc"
	}

	reg = bit.band(reg,0x0F)

	if switch[reg] ~= nil then
		result = self[switch[reg]]
	else
		if     reg ==  8 then
			result = bit.bor(0xff00,self.a)
		elseif reg ==  9 then
			result = bit.bor(0xff00,self.b)
		elseif reg == 10 then
			result = bit.bor(bit.lshift(self.cc.all,8),self.cc.all)
		elseif reg == 11 then
			result = bit.bor(bit.lshift(self.dp,8),self.dp)
		end
	end

	return result
end

function m6809:read_exg_168_register(reg)
	local result = 0xffff

	local switch = {
		[0] = "d",
		[1] = "x",
		[2] = "y",
		[3] = "u",
		[4] = "s",
		[5] = "pc"
	}

	reg = bit.band(reg,0x0F)

	if switch[reg] ~= nil then
		result = self[switch[reg]]
	else
		if     reg == 8 then
			result = bit.bor(0xff00,self.a)
		elseif reg == 9 then
			result = bit.bor(0xff00,self.b)
		elseif reg == 10 then
			result = bit.bor(0xff00,self.cc.all)
		elseif reg == 11 then
			result = bit.bor(0xff00,self.dp)
		end
	end

	return result
end

function m6809:write_exgtfr_register(reg,value)
	local switch = {
		[0] = "d",
		[1] = "x",
		[2] = "y",
		[3] = "u",
		[4] = "s",
		[5] = "pc",
		[8] = "a",
		[9] = "b",
		[10] = "all",
		[11] = "dp",
	}

	reg = bit.band(reg,0x0F)

	if switch[reg] ~= nil then
		self[switch[reg]] = bit.band(2^(8*self.regsize[switch[reg]])-1,value)
	end
end

m6809.exg = function(self)
	local param = self:fetch_operand()
	local reg1, reg2

	if btst(param,7) then
		reg1 = self:read_tfr_exg_816_register(bit.rshift(param,4));
		reg2 = self:read_tfr_exg_816_register(bit.rshift(param,0));
	else
		reg1 = self:read_exg_168_register(bit.rshift(param,4));
		reg2 = self:read_exg_168_register(bit.rshift(param,0));
	end

	self:write_exgtfr_register(bit.rshift(param,0), reg1);
	self:write_exgtfr_register(bit.rshift(param,4), reg2);
end

m6809.tfr = function(self)
	local param = self:fetch_operand()
	local reg = self:read_tfr_exg_816_register(bit.rshift(param,4))
	self:write_exgtfr_register(bit.rshift(param,0), reg);

	--[[ -- WTF does this do? Hopefully some timing thing we don't need
	if ((param & 0x0F) == 4)
	{
		m_lds_encountered = true;
	}
	]]
end


m6809.tsta = function(self)
	self:help_tst(self.a)
end
m6809.tstb = function(self)
	self:help_tst(self.b)
end
m6809.tst = function(self)
	local addr = self:fetch_effective_address()
	local m = self:read(addr)
	self:help_tst(m)
	self.cycles += 1
end

m6809.set_addrmap = function(self, map)
	self.addrmap = map -- Store the memory map

	-- Wrap the read method so it calls the MemoryMap's read method with correct self
	self.read = function(cpu_self, addr)
		return self.addrmap:read(addr)
	end

	-- Wrap the write method so it calls the MemoryMap's write method with correct self
	self.write = function(cpu_self, addr, val)
		return self.addrmap:write(addr, val)
	end
end

return m6809
