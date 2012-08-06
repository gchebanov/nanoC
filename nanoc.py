#!/usr/bin/env python3
class Lexer:
	NUM, ID, IF, ELSE, WHILE, DO, LBRA, RBRA, LPAR, RPAR, PLUS, MINUS, LESS, EQ, SEMICOLON, EOF = range(16)
	LEXEMS = ['NUM', 'ID', 'IF', 'ELSE', 'WHILE', 'DO', 'LBRA', 'RBRA', 'LPAR', 'RPAR', 'PLUS', 'MINUS', 'LESS', 'EQ', 'SEMICOLON', 'EOF']
	SYMBOLS = { '{' : LBRA, '}' : RBRA, '=' : EQ, ';' : SEMICOLON, '(' : LPAR, ')' : RPAR, '+' : PLUS, '-' : MINUS, '<' : LESS }
	WORDS = { 'if' : IF, 'else' : ELSE, 'do' : DO, 'while' : WHILE }
	def __init__(self, fin):
		self.fin = fin

	def getc(self):
		return self.fin.read(1)

	def parse(self):
		c = self.getc()
		while c:
			if c.isspace():
				pass
			elif c in self.SYMBOLS:
				yield self.SYMBOLS[c], None
			elif c.isdigit():
				intval = 0
				while c.isdigit():
					intval = intval * 10 + int(c)
					c = self.getc()
				yield self.NUM, intval
				continue
			elif c.isalpha():
				word = []
				while c.isalpha():
					word.append(c)
					c = self.getc()
				word = ''.join(word)
				if word in self.WORDS:
					yield self.WORDS[word], None
				elif len(word) == 1:
					yield self.ID, ord(word) - ord('a')
				else:
					raise SyntaxError('Lexer: Long id:' + word)
				continue
			else:
				raise SyntaxError('Lexer: Unknown symbol: ' + c)
			c = self.getc()
		yield self.EOF, None

class Node:
	def __init__(self, kind, val = None):
		self.kind = kind
		self.val = val or []

	def __repr__(self, ident = 0):
		if self.kind <= 1 or type(self.val) != list:
			return ' ' * ident + Parser.NODES[self.kind] + ' ' + str(self.val)
		return '\n'.join([' ' * ident + Parser.NODES[self.kind]] + [ e.__repr__(ident + 1) for e in self.val])

class Parser:

	NODES = ['VAR', 'CONST', 'ADD', 'SUB', 'LT', 'SET', 'IF', 'IFE', 'WHILE', 'DO', 'PASS', 'SEQ', 'EXPR', 'PROG']
	VAR, CONST, ADD, SUB, LT, SET, IF, IFE, WHILE, DO, PASS, SEQ, EXPR, PROG = range(14)

	def __init__(self, lexems):
		self.iterator = iter(lexems)
		self.sym = next(self.iterator)

	def go(self, lexem):
		if self.sym[0] == lexem:
			if lexem != Lexer.EOF:
				self.sym = next(self.iterator)
			return True
		return False
	def go2(self, lexem):
		if self.sym[0] == lexem:
			if lexem != Lexer.EOF:
				self.sym = next(self.iterator)
		else:
			raise SyntaxError('Parser: "' + Lexer.LEXEMS[lexem] + '" expected, but "' + Lexer.LEXEMS[self.sym[0]] + '" found')

	def statement(self):
		if self.go(Lexer.IF):
			n = Node(Parser.IF)
			n.val.append(self.paren_expr())
			n.val.append(self.statement())
			if self.go(Lexer.ELSE):
				n.kind = Parser.IFE
				n.val.append(self.statement())
		elif self.go(Lexer.WHILE):
			n = Node(Parser.WHILE)
			n.val.append(self.paren_expr())
			n.val.append(self.statement())
		elif self.go(Lexer.DO):
			n = Node(Parser.DO)
			n.val.append(self.statement())
			self.go2(Lexer.WHILE)
			n.val.append(self.paren_expr())
			self.go2(Lexer.SEMICOLON)
		elif self.go(Lexer.SEMICOLON):
			n = Node(Parser.PASS)
		elif self.go(Lexer.LBRA):
			n = Node(Parser.SEQ)
			while not self.go(Lexer.RBRA):
				n.val.append(self.statement())
		else:
			n = Node(Parser.EXPR , self.expr())
			self.go2(Lexer.SEMICOLON)
		return n

	def paren_expr(self):
		self.go2(Lexer.LPAR)
		n = self.expr()
		self.go2(Lexer.RPAR)
		return n

	def expr(self):
		if self.sym[0] != Lexer.ID:
			return self.test()
		n = self.test()
		if n.kind == Parser.VAR and self.go(Lexer.EQ):
			n = Node(Parser.SET, [n, self.expr()])
		return n
	
	def test(self):
		n = self.add()
		if self.go(Lexer.LESS):
			n = Node(Parser.LT, [n, self.add()])
		return n
	
	def add(self):
		n = self.term()
		while True:
			if self.go(Lexer.PLUS):
				n = Node(Parser.ADD, [n, self.term()])
			elif self.go(Lexer.MINUS):
				n = Node(Parser.SUB, [n, self.term()])
			else:
				break
		return n

	def term(self):
		if self.sym[0] == Lexer.ID:
			n = Node(Parser.VAR, self.sym[1])
			self.go(Lexer.ID)
		elif self.sym[0] == Lexer.NUM:
			n = Node(Parser.CONST, self.sym[1])
			self.go(Lexer.NUM)
		else:
			n = self.paren_expr()
		return n

	def program(self):
		n = Node(Parser.PROG, self.statement())
		self.go2(Lexer.EOF)
		return n

class VM:
	FETCH, STORE, PUSH, POP, ADD, SUB, LT, JZ, JNZ, JMP, PASS = range(11)

class VMRunner:
	def run(self, prog, args):
		if min(prog) < 0 or max(prog) >= 10:
			raise SyntaxError('VM: unknow command')
		if len(prog) != len(args):
			raise SyntaxError('VM: bad len of program')
		for i in range(len(prog)):
			if (prog[i] == VM.JMP or prog[i] == VM.JZ or prog[i] == VM.JNZ) and (args[i] < 0 or args[i] >= len(prog)):
				raise SyntaxError('VM: bad jump')
			elif (prog[i] == VM.FETCH or prog[i] == VM.STORE) and (args[i] < 0 or args[i] >= 26):
				raise SyntaxError('VM: out memory used')
		var = [0] * 26
		stack = []
		ip = 0
		while ip < len(prog):
			op, arg = prog[ip], args[ip]
			if op == VM.FETCH: stack.append(var[arg]);
			elif op == VM.STORE: var[arg] = stack.pop();
			elif op == VM.PUSH: stack.append(arg);
			elif op == VM.POP: stack.pop();
			elif op == VM.ADD: stack[-2] += stack[-1]; stack.pop()
			elif op == VM.SUB: stack[-2] -= stack[-1]; stack.pop()
			elif op == VM.LT: stack[-2] = (1 if stack[-2] < stack[-1] else 0); stack.pop()
			elif op == VM.JZ:
				if not stack.pop(): ip = arg; continue;
			elif op == VM.JNZ:
				if stack.pop(): ip = arg; continue;
			elif op == VM.JMP: ip = arg; continue
			elif op == VM.PASS: pass;
			else: break;
			ip += 1
		return '\n'.join(['%c=%d' % (chr(i+ord('a')), var[i]) for i in range(26) if var[i]])

class Compiler :
	def __init__(self):
		self.prog = []
		self.args = []
		self.l = 0
	def gen(self, command, arg = 0):
		self.prog.append(command)
		self.args.append(arg)
		self.l += 1
	def ncompile(self, nodes):
		for n in nodes:
			self.compile(n)
	def compile(self, n):
		if n.kind == Parser.VAR: self.gen(VM.FETCH, n.val);
		elif n.kind == Parser.CONST: self.gen(VM.PUSH, n.val);
		elif n.kind == Parser.ADD: self.ncompile(n.val); self.gen(VM.ADD);
		elif n.kind == Parser.SUB: self.ncompile(n.val); self.gen(VM.SUB);
		elif n.kind == Parser.LT: self.ncompile(n.val); self.gen(VM.LT);
		elif n.kind == Parser.SET: self.compile(n.val[1]); self.gen(VM.STORE, (n.val[0]).val);
		elif n.kind == Parser.SEQ: self.ncompile(n.val);
		elif n.kind == Parser.EXPR: self.compile(n.val); self.gen(VM.POP, 0);
		elif n.kind == Parser.PROG: self.compile(n.val); self.gen(VM.PASS, 0);
		elif n.kind == Parser.IF:
			self.compile(n.val[0])
			self.gen(VM.JZ, 0); addr = self.l
			self.compile(n.val[1])
			self.args[addr] = self.l
		elif n.kind == Parser.IFE:
			self.compile(n.val[0])
			self.gen(VM.JZ, 0); addr1 = self.l;
			self.compile(n.val[1])
			self.gen(VM.JMP, 0); addr2 = self.l;
			self.args[addr1] = self.l
			self.compile(n.val[2]); self.args[addr2] = self.l
		elif n.kind == Parser.WHILE:
			addr1 = self.l
			self.compile(n.val[0])
			self.gen(VM.JZ, 0); addr2 = self.l
			self.compile(n.val[1])
			self.gen(VM.JMP, addr1)
			self.args[addr2] = self.l
		elif n.kind == Parser.DO: addr = self.l; self.ncompile(n.val); self.gen(VM.JNZ, addr);
		else:
			raise SyntaxError('Compiler:Node.kind unknown' + str(n.kind))
		return self.prog, self.args

if __name__ == "__main__":
	import io
	tests = [
		(" i =   3;", "i=3"),
		("{ a=3; b=5;}", "a=3\nb=5"),
		("{ a = 1; b = 2; c = a + b; }", "a=1\nb=2\nc=3"),
		("{ a = 5; b = 2; c = a - b; }", "a=5\nb=2\nc=3"),
		("{ a = 5; b = 2; c = a < b; }", "a=5\nb=2\nc=0"),
		("{ a = 5; if(a<10) a = 33; }", "a=33"),
		("{ a = 5; if(10<a) a = 33; else {a = 1; b = 2;} }", "a=33"),
		("{ a = 10; do { a = a - 2;} while(3 < a); }","a=2" ),
	]
	for prog, ans in tests:
		print(prog)
#		for lexem in Lexer(io.StringIO(prog)).parse():
#			print(lexem[1] if lexem[1] else Lexer.LEXEMS[lexem[0]])
#		print(repr(Parser(Lexer(io.StringIO(prog)).parse()).statement()));
		l = Lexer(io.StringIO(prog)).parse()
		print(list(l))
#		p = Parser(l).program()
#		c = Compiler().compile(p)
#		print(repr(p))
#		print('\n'.join(map(str, c)))
		print(ans)

