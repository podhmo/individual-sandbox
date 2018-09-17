from IPython.utils.timing import clock, clock2

    def time(self,line='', cell=None, local_ns=None):
        expr = self.shell.input_transformer_manager.transform_cell(cell)

        ## parse ast
        tp_min = 0.1
        t0 = clock()
        expr_ast = self.shell.compile.ast_parse(expr)
        tp = clock()-t0

        ## compile
        tc_min = 0.1
        t0 = clock()
        code = self.shell.compile(expr_ast, source, mode)
        tc = clock()-t0

        ## execute
        st = clock2()
        try:
            exec(code, glob, local_ns)
        except:
            self.shell.showtraceback()
            return
        end = clock2()

        # Compute actual times and report
        cpu_user = end[0]-st[0]
        cpu_sys = end[1]-st[1]
        cpu_tot = cpu_user+cpu_sys
        print("CPU times: user %s, sys: %s, total: %s" % (_format_time(cpu_user),_format_time(cpu_sys),_format_time(cpu_tot)))
        if tc > tc_min:
            print("Compiler : %s" % _format_time(tc))
        if tp > tp_min:
            print("Parser   : %s" % _format_time(tp))
