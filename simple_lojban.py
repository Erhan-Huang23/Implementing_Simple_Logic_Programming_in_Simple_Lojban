import re, sys

TOK_I, TOK_NAME, TOK_NUM, TOK_SHORT, TOK_PRED = 'I','NAME','NUM','SHORT','PRED'
name_re  = re.compile(r"\.[A-Za-z]+\.")
num_re   = re.compile(r"0|[1-9][0-9]*")
alpha_re = re.compile(r"[A-Za-z]+")
CV = set("bcdfghjklmnpqrstvwxyz")
V  = set("aeiou")

def is_gismu(s: str) -> bool:
    if len(s) != 5 or not s.isalpha(): return False
    s = s.lower()
    c = lambda ch: ch in CV
    v = lambda ch: ch in V
    return (c(s[0]) and v(s[1]) and c(s[2]) and c(s[3]) and v(s[4])) or \
           (c(s[0]) and c(s[1]) and v(s[2]) and c(s[3]) and v(s[4]))

def scan(src: str):
    tks, i, s = [], 0, src
    while i < len(s):
        if s[i].isspace(): i += 1; continue
        start, rest = i, s[i:]

        if s[i] in 'iI' and (i == 0 or s[i-1].isspace()):
            tks.append((TOK_I, 'i', i)); i += 1; continue

        m = name_re.match(rest)
        if m: w = m.group(0); tks.append((TOK_NAME, w, start)); i += len(w); continue

        m = num_re.match(rest)
        if m: w = m.group(0); tks.append((TOK_NUM, w, start)); i += len(w); continue

        m = alpha_re.match(rest)
        if m:
            w = m.group(0).lower()
            if is_gismu(w): tks.append((TOK_PRED, w, start))
            elif len(w) == 2 and w[0] in CV and w[1] in V: tks.append((TOK_SHORT, w, start))
            else: tks.append((TOK_PRED, w, start))
            i += len(w); continue

        raise SyntaxError(f"Unexpected char at {start}: {s[i]}")
    return tks

VVAR, VNUM, VNIL, VCONS = 'var','num','nil','cons'
nil = (VNIL,)

def Var(n):  return (VVAR, n)
def Num(v):  return (VNUM, int(v))
def Cons(h,t): return (VCONS, h, t)

def isvar(x):  return x and x[0] == VVAR
def isnum(x):  return x and x[0] == VNUM
def iscons(x): return x and x[0] == VCONS

def term_str(t):
    k = t[0]
    if k == VVAR: return f".{t[1]}."
    if k == VNUM: return str(t[1])
    if k == VNIL: return "lo steni"
    if k == VCONS:
        xs, u = [], t
        while u[0] == VCONS:
            xs.append(term_str(u[1])); u = u[2]
        return "( " + " ".join(xs) + " )"
    return str(t)

class P:
    def __init__(self, tks): self.t, self.k = tks, 0
    def done(self): return self.k >= len(self.t)
    def peek(self): return None if self.done() else self.t[self.k]
    def eat(self, typ=None):
        if self.done(): raise SyntaxError("Unexpected EOF")
        tok = self.t[self.k]
        if typ and tok[0] != typ: raise SyntaxError(f"Expected {typ}, got {tok[0]} ('{tok[1]}') at {tok[2]}")
        self.k += 1; return tok

    def parse(self):
        calls = []
        while not self.done():
            self.eat(TOK_I)
            calls.append(self.stmt())
        return calls

    def stmt(self):
        swap = False
        if self._acc_short('se'): swap = not swap
        a1 = self.arg()
        name = self.eat(TOK_PRED)[1]
        if self._acc_short('se'): swap = not swap
        args = [a1]
        while len(args) < 5 and self._can_arg(): args.append(self.arg())
        if swap and len(args) >= 2: args[0], args[1] = args[1], args[0]
        return (name, args)

    def _acc_short(self, s):
        t = self.peek()
        if t and t[0] == TOK_SHORT and t[1] == s: self.eat(); return True
        return False

    def _can_arg(self):
        t = self.peek(); return bool(t and t[0] in (TOK_NUM, TOK_NAME, TOK_SHORT, TOK_PRED))

    def arg(self):
        t = self.peek()
        if t[0] == TOK_NUM: self.eat(); return Num(t[1])
        if t[0] == TOK_SHORT and t[1] == 'lo':
            self.eat(); u = self.peek()
            if not u: raise SyntaxError("'lo' expects a term")
            if u[0] == TOK_NAME: self.eat(); return Var(u[1].strip('.'))
            if u[0] == TOK_PRED and u[1] == 'steni': self.eat(); return nil
            if u[0] == TOK_PRED and u[1] == 'steko': return self.list_lit()
            if u[0] == TOK_PRED: self.eat(); return Var(u[1])
            raise SyntaxError("Bad token after 'lo'")
        if t[0] == TOK_NAME: self.eat(); return Var(t[1].strip('.'))
        raise SyntaxError(f"Bad arg {t}")

    def list_lit(self):
        self.eat(TOK_PRED)  
        xs = []

        def elem():
            u = self.peek()
            if not u: raise SyntaxError("Unterminated list; need 'lo steni'")
            if u[0] == TOK_NUM: self.eat(); return Num(u[1])
            if u[0] == TOK_NAME: self.eat(); return Var(u[1].strip('.'))
            if u[0] == TOK_SHORT and u[1] == 'lo':
                self.eat(); v = self.peek()
                if not v: raise SyntaxError("'lo' in list needs a following term")
                if v[0] == TOK_NAME: self.eat(); return Var(v[1].strip('.'))
                if v[0] == TOK_PRED and v[1] == 'steko': return self.list_lit()
                if v[0] == TOK_PRED and v[1] == 'steni': raise SyntaxError("empty element before steni")
                if v[0] == TOK_PRED: self.eat(); return Var(v[1])
            raise SyntaxError("Bad list element")

        xs.append(elem())
        while True:
            t = self.peek()
            if not t: raise SyntaxError("Unterminated list; need 'lo steni'")
            if t[0] == TOK_SHORT and t[1] == 'lo':
                self.eat(); u = self.peek()
                if not u: raise SyntaxError("'lo' must be followed by steni/steko/NAME/id")
                if u[0] == TOK_PRED and u[1] == 'steni':
                    self.eat(); return cons_list(xs)
                if u[0] == TOK_PRED and u[1] == 'steko':
                    self.eat(); xs.append(elem()); continue
                if u[0] == TOK_NAME:
                    self.eat(); xs.append(Var(u[1].strip('.'))); continue
                if u[0] == TOK_PRED:
                    self.eat(); xs.append(Var(u[1])); continue
                raise SyntaxError("Bad token after 'lo' in list")
            else:
                raise SyntaxError("Bad list element (expecting 'lo ...' or 'lo steni')")

def cons_list(xs):
    t = nil
    for x in reversed(xs): t = Cons(x, t)
    return t

def list_items(t):
    out = []
    while t[0] == VCONS: out.append(t[1]); t = t[2]
    if t[0] != VNIL: raise SyntaxError("List must end with steni")
    return out

class KB:
    def __init__(self):
        self.facts = {}   
        self.rules = {}   
        self._gid = 0

    
    def _walk(self, t, env):
        while isvar(t) and t[1] in env: t = env[t[1]]
        return t

    def _unify(self, a, b, env):
        a = self._walk(a, env); b = self._walk(b, env)
        if isvar(a): env[a[1]] = b; return True
        if isvar(b): env[b[1]] = a; return True
        if isnum(a) and isnum(b): return a[1] == b[1]
        if a[0] == VNIL and b[0] == VNIL: return True
        if a[0] == VCONS and b[0] == VCONS:
            return self._unify(a[1], b[1], env) and self._unify(a[2], b[2], env)
        return False

    def _unify_list(self, xs, ys, env):
        if len(xs) != len(ys): return False
        for x, y in zip(xs, ys):
            if not self._unify(x, y, env): return False
        return True

    
    def _as_int(self, t): return t[1] if isnum(t) else None

    def _builtin(self, name, args):
        if name == 'fatci':
            if len(args) != 1: return []
            a = args[0]
            return [{}] if not isvar(a) else [{a[1]: a}]
        if name == 'steni':
            if len(args) != 1: return []
            e = {}; return [e] if self._unify(args[0], nil, e) else []
        if name == 'steko':
            if len(args) == 2:
                x, h = args; e = {}; return [e] if self._unify(x, Cons(h, nil), e) else []
            if len(args) == 3:
                x, h, t = args; e = {}; return [e] if self._unify(x, Cons(h, t), e) else []
            return []
        if name in ('sumji', 'vujni'):
            if len(args) != 3: return []
            a, b, c = args
            B, C = self._as_int(b), self._as_int(c)
            out = []
            if B is not None and C is not None:
                val = B + C if name == 'sumji' else B - C
                e = {}; 
                if self._unify(a, Num(val), e): out.append(e)
            else:
                A = self._as_int(a)
                if A is not None:
                    if B is not None:
                        val = A - B if name == 'sumji' else B - A
                        e = {}; 
                        if self._unify(c, Num(val), e): out.append(e)
                    elif C is not None:
                        val = A - C if name == 'sumji' else A + C
                        e = {}; 
                        if self._unify(b, Num(val), e): out.append(e)
            return out
        if name == 'dunli':
            if len(args) != 2: return []
            e = {}; return [e] if self._unify(args[0], args[1], e) else []
        return None  

    
    def add_fact(self, name, args): self.facts.setdefault(name, []).append(tuple(args))
    def add_rule(self, name, head, body): self.rules.setdefault(name, []).append((head, body))

    def _fresh(self, head, body):
        self._gid += 1; suf = f"__{self._gid}"; mp = {}
        def rt(t):
            if isvar(t):
                mp.setdefault(t[1], Var(t[1] + suf)); return mp[t[1]]
            if t[0] == VCONS: return Cons(rt(t[1]), rt(t[2]))
            return t
        def rc(c): return (c[0], [rt(a) for a in c[1]])
        return [rt(x) for x in head], [rc(c) for c in body]

    def query(self, goal):
        name, args = goal
        b = self._builtin(name, args)
        if b is not None: return b
        out = []
        def dfs(goals, env):
            if not goals: out.append(env.copy()); return
            (gname, gargs), rest = goals[0], goals[1:]
            bb = self._builtin(gname, gargs)
            if bb is not None:
                for e2 in merge(env, bb): dfs(rest, e2)
                return
            for tup in self.facts.get(gname, []):
                if len(tup) != len(gargs): continue
                e2 = env.copy()
                if self._unify_list(gargs, list(tup), e2): dfs(rest, e2)
            for head, body in self.rules.get(gname, []):
                H, B = self._fresh(head, body)
                e2 = env.copy()
                if self._unify_list(gargs, H, e2): dfs(B + rest, e2)
        dfs([goal], {})
        return out

def merge(env, sols):
    outs = []
    for s in sols:
        e, ok = env.copy(), True
        for k, v in s.items():
            if k in e:
                kb = KB(); tmp = {}
                if not kb._unify(e[k], v, tmp): ok = False; break
            e[k] = v
        if ok: outs.append(e)
    return outs


def body_item_to_call(lst_terms):
    
    if len(lst_terms) < 2 or not isvar(lst_terms[1]):
        raise SyntaxError("Rule body must be ( arg1  <pred>  ... )")
    return (lst_terms[1][1], [lst_terms[0]] + lst_terms[2:])


def eval_program(src: str):

    src = re.sub(r'(?<=[A-Za-z0-9.])(?=\bi\b)', ' ', src)

    tks   = scan(src.lower())
    calls = P(tks).parse()
    kb    = KB()

    for name, args in calls[:-1]:
        if name == 'fatci':
            kb.query((name, args))
        elif name == 'cmavo':
            if len(args) < 2 or not isvar(args[0]):
                raise SyntaxError("cmavo: need predicate name and arg list")
            pred = args[0][1]
            head = list_items(args[1])
            if len(args) >= 3:
                body = [] if args[2][0] == 'nil' else [
                    body_item_to_call(list_items(x)) for x in list_items(args[2])
                ]
            else:
                body = []
            kb.add_rule(pred, head, body)
        else:
            kb.add_fact(name, args)


    return kb.query(calls[-1])

def main():
    if sys.stdin.isatty():
        print("Enter statements (blank line to finish):")
        lines = []
        while True:
            try:
                line = input()
            except EOFError:
                break
            if not line.strip(): break
            lines.append(line)
        src = "\n".join(lines)
    else:
        src = sys.stdin.read()

    sols = eval_program(src)
    if not sols:
        print("false"); return
    for i, env in enumerate(sols, 1):
        pairs = [f"{k} = {term_str(v)}" for k, v in env.items()]
        print(f"{i}: " + (", ".join(pairs) if pairs else "true"))

if __name__ == "__main__":
    main()
