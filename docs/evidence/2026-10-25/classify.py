"""Apply the design's document-level rule signatures (section 3.4) literally to every extracted table."""
import json, re, collections
E='/private/tmp/claude-501/-Users-scott-hsed/f05c341b-63d7-4ac7-8e79-f36321a58c77/scratchpad/extract/'
core=json.load(open(E+'core_tables.json'))['tables']
opal=json.load(open(E+'opal_tables.json'))['tables']
norm=lambda s: re.sub(r'\s+','',s).lower()
def header(t):
    return [norm(c['text']) for c in t['header_rows'][0]] if t['header_rows'] else []
def first_row(t):
    return [norm(c['text']) for c in t['rows'][0]] if t['rows'] else []
# Core section scopes by 1-based PDF page (spec-tables fact 4: printed+17)
def core_rules(t):
    p=t['pages'][0]+1; cap=t['caption']; h=header(t); fr=first_row(t)
    out=[]
    in_513 = 100<=p<=127
    in_s3 = 28<=p<=90      # §3 Architecture Elements p11..73 printed
    in_336 = 67<=p<=72     # §3.3.6 Level 0 discovery
    in_515 = p==137
    in_53_58 = 154<=p<=299
    in_s6 = 300<=p<=306
    if in_513 and h==['uid','name','format'] and re.match(r'^Table \d+\s+\S+$',cap): out.append('typeRow')
    if in_513 and len(h)==2 and h[0] in ('enumerationvalue','setvalue') and re.search(r'(Enumeration|Set) Values$',cap): out.append('enumValues')
    if in_53_58 and h==['columnnumber','columnname','isunique','columntype'] and cap.endswith('Table Description'): out.append('tableDesc')
    if in_s6 and h==['uidoftabledescriptorobject','uidoftable','tablename','template']: out.append('tableUIDs')
    if in_s6 and len(h)>=2 and h[0]=='uid' and h[1]=='name': out.append('uidTable')
    if in_515 and h==['name','value']: out.append('statusCodes')
    grid = (h[:1] in (['bit'],['bitbyte']) and h[1:]==['7','6','5','4','3','2','1','0']) or (fr[:1]==['bit'] and fr[1:]==['7','6','5','4','3','2','1','0'])
    if in_s3 and grid: out.append('byteGrid')
    if in_336 and grid: out.append('feature')
    return out
def opal_rules(t):
    p=t['pages'][0]+1; cap=t['caption']; h=header(t); fr=first_row(t); out=[]
    grid = (h[:1] in (['bit'],['bitbyte']) and h[1:]==['7','6','5','4','3','2','1','0']) or (fr[:1]==['bit'] and fr[1:]==['7','6','5','4','3','2','1','0'])
    if 19<=p<=28 and grid: out.append('feature')
    if 29<=p<=32 and grid: out.append('byteGrid')
    if p==51 and h==['uid','name','format']: out.append('typeRow')
    if p==51 and len(h)==2 and h[0] in ('enumerationvalue','setvalue'): out.append('enumValues')
    if h==['columnnumber','columnname','isunique','columntype'] and cap.endswith('Table Description'): out.append('tableDesc')
    if 33<=p<=90 and re.match(r'^Table \d+ - (Admin|Locking) SP [-–] \S+ Table Preconfiguration$',cap): out.append('preconfig')
    return out
for name,tabs,rules in (('CORE',core,core_rules),('OPAL',opal,opal_rules)):
    print('#####',name)
    bound=collections.Counter(); unbound=[]; multi=[]
    for t in tabs:
        if t.get('nested_in') or t.get('table_number') is None: continue
        if t.get('part') and str(t['part']).startswith('1'): continue
        r=rules(t)
        if not r: unbound.append((t['table_number'], t['caption'][:60], header(t)[:5] or first_row(t)[:5], f"{t['n_rows']}x{t['n_cols']}"))
        else:
            for x in r: bound[x]+=1
            if len(r)>1: multi.append((t['table_number'],r))
    print('rule-bound counts:', dict(bound))
    print('tables matched by >1 rule:', multi)
    print('UNBOUND (%d):'%len(unbound))
    for u in unbound: print('  ', u)
