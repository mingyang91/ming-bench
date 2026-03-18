#!/usr/bin/env python3
"""Parse token usage from session.jsonl files for benchmark comparison."""
import json
import sys
import os
import glob

def parse_session(jsonl_path):
    """Extract token usage from a session.jsonl file."""
    totals = {
        'input_tokens': 0,
        'output_tokens': 0,
        'cache_creation_input_tokens': 0,
        'cache_read_input_tokens': 0,
    }
    
    if not os.path.exists(jsonl_path):
        return None
    
    with open(jsonl_path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except json.JSONDecodeError:
                continue
            
            # Navigate to usage
            usage = None
            if 'message' in obj and isinstance(obj['message'], dict):
                usage = obj['message'].get('usage')
            
            if usage and isinstance(usage, dict):
                totals['input_tokens'] += usage.get('input_tokens', 0)
                totals['output_tokens'] += usage.get('output_tokens', 0)
                totals['cache_creation_input_tokens'] += usage.get('cache_creation_input_tokens', 0)
                totals['cache_read_input_tokens'] += usage.get('cache_read_input_tokens', 0)
    
    return totals

def process_run(run_dir, label):
    """Process all levels in a run directory."""
    levels = sorted(glob.glob(os.path.join(run_dir, 'L*')))
    
    grand_total = {
        'input_tokens': 0,
        'output_tokens': 0,
        'cache_creation_input_tokens': 0,
        'cache_read_input_tokens': 0,
    }
    
    print(f"\n{'='*70}")
    print(f"  {label}")
    print(f"{'='*70}")
    print(f"{'Level':<8} {'Input':>10} {'Output':>10} {'Cache Write':>12} {'Cache Read':>12} {'Status'}")
    print(f"{'-'*8} {'-'*10} {'-'*10} {'-'*12} {'-'*12} {'-'*10}")
    
    for level_dir in levels:
        level_name = os.path.basename(level_dir)
        session_file = os.path.join(level_dir, 'session.jsonl')
        status_file = os.path.join(level_dir, 'status.txt')
        
        status = "running..."
        if os.path.exists(status_file):
            with open(status_file) as f:
                status = f.read().strip()
        
        tokens = parse_session(session_file)
        if tokens:
            for k in grand_total:
                grand_total[k] += tokens[k]
            print(f"{level_name:<8} {tokens['input_tokens']:>10,} {tokens['output_tokens']:>10,} {tokens['cache_creation_input_tokens']:>12,} {tokens['cache_read_input_tokens']:>12,} {status}")
        else:
            print(f"{level_name:<8} {'—':>10} {'—':>10} {'—':>12} {'—':>12} {status}")
    
    print(f"{'-'*8} {'-'*10} {'-'*10} {'-'*12} {'-'*12}")
    print(f"{'TOTAL':<8} {grand_total['input_tokens']:>10,} {grand_total['output_tokens']:>10,} {grand_total['cache_creation_input_tokens']:>12,} {grand_total['cache_read_input_tokens']:>12,}")
    
    # Effective tokens (input + cache_write costs 1.25x, cache_read costs 0.1x for billing)
    effective_input = grand_total['input_tokens'] + grand_total['cache_creation_input_tokens'] + grand_total['cache_read_input_tokens']
    # Cost estimate (Opus pricing: $15/M input, $75/M output, cache write $18.75/M, cache read $1.50/M)
    cost_input = grand_total['input_tokens'] * 15 / 1_000_000
    cost_output = grand_total['output_tokens'] * 75 / 1_000_000
    cost_cache_write = grand_total['cache_creation_input_tokens'] * 18.75 / 1_000_000
    cost_cache_read = grand_total['cache_read_input_tokens'] * 1.50 / 1_000_000
    total_cost = cost_input + cost_output + cost_cache_write + cost_cache_read
    
    print(f"\n  Total tokens: {sum(grand_total.values()):,}")
    print(f"  Estimated cost: ${total_cost:.2f}")
    print(f"    Input: ${cost_input:.2f} | Output: ${cost_output:.2f} | Cache Write: ${cost_cache_write:.2f} | Cache Read: ${cost_cache_read:.2f}")
    
    return grand_total

base_dir = '/home/my/.zeroclaw/workspace/cs61a-bench/results'
main_dir = os.path.join(base_dir, 'main_claude-main-levels_20260318T072506')
strat_dir = os.path.join(base_dir, 'strategy_claude-strat-levels_20260318T072506')

main_totals = process_run(main_dir, '🔵 MAIN branch (claude-main-levels)')
strat_totals = process_run(strat_dir, '🟢 STRATEGY branch (claude-strat-levels)')

# Comparison
print(f"\n{'='*70}")
print(f"  📊 COMPARISON")
print(f"{'='*70}")
print(f"{'Metric':<25} {'Main':>15} {'Strategy':>15} {'Diff':>15}")
print(f"{'-'*25} {'-'*15} {'-'*15} {'-'*15}")

for key in ['input_tokens', 'output_tokens', 'cache_creation_input_tokens', 'cache_read_input_tokens']:
    m = main_totals[key]
    s = strat_totals[key]
    diff = s - m
    pct = f"({diff/m*100:+.1f}%)" if m > 0 else ""
    label = key.replace('_', ' ').title()
    print(f"{label:<25} {m:>15,} {s:>15,} {diff:>+15,} {pct}")

m_total = sum(main_totals.values())
s_total = sum(strat_totals.values())
diff_total = s_total - m_total
pct_total = f"({diff_total/m_total*100:+.1f}%)" if m_total > 0 else ""
print(f"{'Total':<25} {m_total:>15,} {s_total:>15,} {diff_total:>+15,} {pct_total}")
