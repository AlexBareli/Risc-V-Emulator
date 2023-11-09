#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "rv_emu.h"
#include "bits.h"

#define DEBUG 0

static void unsupported(char *s, uint32_t n) {
    printf("unsupported %s 0x%x\n", s, n);
    exit(-1);
}

void emu_r_type(rv_state *state, uint32_t iw) {
    uint32_t rd = (iw >> 7) & 0b11111;
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t rs2 = (iw >> 20) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;
    uint32_t funct7 = (iw >> 25) & 0b1111111;

    switch (funct3) {
        case 0b000:
            switch (funct7) {
                case 0b0000000:
                    state->regs[rd] = state->regs[rs1] + state->regs[rs2];
                    break;
                case 0b0000001:
                    state->regs[rd] = state->regs[rs1] * state->regs[rs2];
                    break;
                case 0b0100000:
                    state->regs[rd] = state->regs[rs1] - state->regs[rs2];
                    break;
                default:
                    unsupported("R-type " + funct3, funct7);
                    break;
            }
            break;
        case 0b101:
            switch(funct7) {
                case 0b0000000:
                    state->regs[rd] = state->regs[rs1] >> state->regs[rs2];
                    break;
                default:
                    unsupported("R-type " + funct3, funct7);
                    break;
            }
            break;
        case 0b001:
            switch(funct7) {
                case 0b0000000:
                    state->regs[rd] = state->regs[rs1] << state->regs[rs2];
                    break;
                default:
                    unsupported("R-type " + funct3, funct7);
                    break;
            }
            break;
        case 0b111:
            switch(funct7) {
                case 0b0000000:
                    state->regs[rd] = state->regs[rs1] & state->regs[rs2];
                    break;
                default:
                    unsupported("R-type ", funct7);
                    break;
            }
            break;
        default:
            unsupported("R-type ", funct3);
            break;
        }
    state->analysis.ir_count += 1;
    state->analysis.i_count += 1;
    state->pc += 4;
}

void emu_b_type(rv_state *state, uint32_t iw) {
    uint32_t bit11 = (iw >> 7) & 0b1;
    uint32_t bit12 = (iw >> 31) & 0b1;
    uint32_t bit4_1 = (iw >> 8) & 0b1111;
    uint32_t bit10_5 = (iw >> 25) & 0b111111;
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t rs2 = (iw >> 20) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;

    uint64_t imm = (bit12 << 12) | (bit11 << 11) | (bit10_5 << 5) | (bit4_1 << 1);
    uint64_t shift = imm << 51;
    int64_t shiftr = ((int64_t) shift) >> 51;

    switch(funct3) {
        case 0b100:
            if((int) state->regs[rs1] < (int) state->regs[rs2]) {
                state->analysis.b_taken += 1;
                state->pc += shiftr;
            } else {
                state->analysis.b_not_taken += 1;
                state->pc += 4;
            }
            break;
        case 0b001:
            if((int) state->regs[rs1] != (int) state->regs[rs2]) {
                state->analysis.b_taken += 1;
                state->pc += shiftr;
            } else {
                state->analysis.b_not_taken += 1;
                state->pc += 4;
            }
            break;
        case 0b101:
            if((int) state->regs[rs1] >= (int) state->regs[rs2]) {
                state->analysis.b_taken += 1;
                state->pc += shiftr;
            } else {
                state->analysis.b_not_taken += 1;
                state->pc += 4;
            }
            break;
        case 0b000:
            if((int) state->regs[rs1] == (int) state->regs[rs2]) {
                state->analysis.b_taken += 1;
                state->pc += shiftr;
            } else {
                state->analysis.b_not_taken += 1;
                state->pc += 4;
            }
            break;
        default:
            unsupported("B-type ", funct3);
    }
    state->analysis.i_count += 1;
}

void emu_load(rv_state *state, uint32_t iw){
    uint32_t rd = (iw >> 7) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint64_t imm = (iw >> 20) & 0b11111111111;
    
    uint64_t shift = imm << 52;
    int64_t shiftr = ((int64_t) shift) >> 52;

    uint64_t base = state->regs[rs1];

    switch(funct3){
        case 0b011:
            int64_t *double_pointer = (int64_t *)(base + shiftr);
            state->regs[rd] = (int64_t) *double_pointer;
            break;
        case 0b000:
            int8_t *char_pointer = (int8_t *)(base + shiftr);
            state->regs[rd] = (int8_t) *char_pointer;
            break;
        case 0b010:
            int32_t *pointer = (int32_t *)(base + shiftr);
            state->regs[rd] = (int32_t) *pointer;
            break;
        default:
            unsupported("Load ", funct3);
    }
    state->analysis.ld_count += 1;
    state->analysis.i_count += 1;
    state->pc += 4;
}

void emu_store(rv_state *state, uint32_t iw){
    uint32_t bit0_4 = (iw >> 7) & 0b11111;
    uint32_t bit11_5 = (iw >> 25) & 0b1111111;
    uint32_t funct3 = (iw >> 12) & 0b111;
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t rs2 = (iw >> 20) & 0b11111;

    uint64_t imm = (bit11_5 << 5) | bit0_4;
    uint64_t shift = imm << 52;
    int64_t shiftr = ((int64_t) shift) >> 52;

    uint64_t base = state->regs[rs1];
  
    switch(funct3) {
        case 0b011:
            int64_t *double_pointer = (int64_t*)(base + shiftr);
            *double_pointer = (int64_t) state->regs[rs2];
            break;
        case 0b000:
            int8_t *char_pointer = (int8_t*)(base + shiftr);
            *char_pointer = (int8_t) state->regs[rs2];
            break;
        case 0b010:
            int32_t *pointer = (int32_t*)(base + shiftr);
            *pointer = (int32_t) state->regs[rs2];
            break;
        default:
            unsupported("Store ", funct3);
    }
    state->analysis.st_count += 1;
    state->analysis.i_count += 1;
    state->pc += 4;
}

void emu_jalr(rv_state *state, uint32_t iw){
    uint32_t rs1 = (iw >> 15) & 0b1111;
    uint64_t val = state->regs[rs1];
    state->analysis.j_count += 1;
    state->analysis.i_count += 1;
    state->pc = val;
}

void emu_jal(rv_state *state, uint32_t iw){
    uint32_t rd = (iw >> 7) & 0b11111;
    uint32_t bit19_12 = (iw >> 12) & 0b11111111;
    uint32_t bit11 = (iw >> 20) & 0b1;
    uint32_t bit10_1 = (iw >> 21) & 0b1111111111;
    uint32_t bit20 = (iw >> 31) & 0b1;

    uint64_t imm = (bit20 << 20) | (bit19_12 << 12) | (bit11 << 11) | (bit10_1 << 1);
    uint64_t shift = imm << 43;
    int64_t shiftr = ((int64_t) shift) >> 43;
    
    if(rd != 0) {
        state->regs[rd] = state->pc + 4;
    }
    state->analysis.j_count += 1;
    state->analysis.i_count += 1;
    state->pc += shiftr;
}

void emu_shift(rv_state *state, uint32_t iw) {
    uint32_t rd = (iw >> 7) & 0b11111;
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t shamt = (iw >> 20) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;
    uint32_t funct7 = (iw >> 25) & 0b1111111;

    switch(funct3) {
        case 0b001:
            if(funct7 == 0b0000000) {
                state->regs[rd] = state->regs[rs1] << shamt;
            } else {
                unsupported("shift type " + funct3, funct7);
            }
            break;
        case 0b101:
            if(funct7 == 0b0000000) {
                state->regs[rd] = state->regs[rs1] >> shamt;
            } else {
                unsupported("Shift type " + funct3, funct7);
            }
            break;
        case 0b000:
            uint64_t imm = (iw >> 20) & 0b111111111111;
            uint64_t shift = imm << 52;
            int64_t shiftr = ((int64_t) shift) >> 52;
            state->regs[rd] = state->regs[rs1] + shiftr;
            break;
        default:
            unsupported("Shift type", funct3);
        }
    state->analysis.ir_count += 1;
    state->analysis.i_count += 1;
    state->pc += 4;
}

static void rv_one(rv_state *state) {
    uint32_t iw  = *((uint32_t*) state->pc);
    iw = cache_lookup(&state->i_cache, (uint64_t) state->pc);

    uint32_t opcode = get_bits(iw, 0, 7);


#if DEBUG
    printf("iw: %x\n", iw);
#endif

    switch (opcode) {
        case 0b0110011:
            emu_r_type(state, iw);
            break;
        case 0b1100111:
            emu_jalr(state, iw);
            break;
        case 0b0010011:
            emu_shift(state, iw);
            break;
        case 0b1100011:
            emu_b_type(state, iw);
            break;
        case 0b1101111:
            emu_jal(state, iw);
            break;
        case 0b0100011:
            emu_store(state, iw);
            break;
        case 0b0000011:
            emu_load(state, iw);
            break;
        default:
            unsupported("Unknown opcode: ", opcode);
    }
}

void rv_init(rv_state *state, uint32_t *target, 
             uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3) {
    state->pc = (uint64_t) target;
    state->regs[RV_A0] = a0;
    state->regs[RV_A1] = a1;
    state->regs[RV_A2] = a2;
    state->regs[RV_A3] = a3;

    state->regs[RV_ZERO] = 0;  // zero is always 0  (:
    state->regs[RV_RA] = RV_STOP;
    state->regs[RV_SP] = (uint64_t) &state->stack[STACK_SIZE];

    memset(&state->analysis, 0, sizeof(rv_analysis));
    cache_init(&state->i_cache);
}

uint64_t rv_emulate(rv_state *state) {
    while (state->pc != RV_STOP) {
        rv_one(state);
    }
    return state->regs[RV_A0];
}

static void print_pct(char *fmt, int numer, int denom) {
    double pct = 0.0;

    if (denom)
        pct = (double) numer / (double) denom * 100.0;
    printf(fmt, numer, pct);
}

void rv_print(rv_analysis *a) {
    int b_total = a->b_taken + a->b_not_taken;

    printf("=== Analysis\n");
    print_pct("Instructions Executed  = %d\n", a->i_count, a->i_count);
    print_pct("R-type + I-type        = %d (%.2f%%)\n", a->ir_count, a->i_count);
    print_pct("Loads                  = %d (%.2f%%)\n", a->ld_count, a->i_count);
    print_pct("Stores                 = %d (%.2f%%)\n", a->st_count, a->i_count);    
    print_pct("Jumps/JAL/JALR         = %d (%.2f%%)\n", a->j_count, a->i_count);
    print_pct("Conditional branches   = %d (%.2f%%)\n", b_total, a->i_count);
    print_pct("  Branches taken       = %d (%.2f%%)\n", a->b_taken, b_total);
    print_pct("  Branches not taken   = %d (%.2f%%)\n", a->b_not_taken, b_total);
}
