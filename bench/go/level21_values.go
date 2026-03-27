package ming

type multiValuesValue struct {
	items []value
}

type callWithValuesProc struct {
	name string
}

type callWithValuesProducerFrame struct {
	consumer procedure
	pos      SourcePos
	next     continuation
}

type multiValueContinuation interface {
	continuation
	acceptsMultipleValues() bool
}

func (v multiValuesValue) schemeString() string {
	return "#<values>"
}

func (multiValuesValue) isTruthy() bool {
	return true
}

func (p callWithValuesProc) schemeString() string {
	return "#<procedure:" + p.name + ">"
}

func (callWithValuesProc) isTruthy() bool {
	return true
}

func (p callWithValuesProc) call(args []value) (value, error) {
	producer, consumer, err := parseCallWithValuesArgs(args, p.name)
	if err != nil {
		return nil, err
	}

	m := machine{}
	if err := startCallWithValues(&m, producer, consumer, currentEvalPos, nil); err != nil {
		return nil, err
	}
	return m.run()
}

func (*callWithValuesProducerFrame) acceptsMultipleValues() bool {
	return true
}

func (f *callWithValuesProducerFrame) resume(m *machine, v value) error {
	return applyProcedureState(m, f.consumer, producedValues(v), f.pos, f.next)
}

func valuesResult(items []value) value {
	if len(items) == 1 {
		return items[0]
	}

	result := make([]value, len(items))
	copy(result, items)
	return multiValuesValue{items: result}
}

func evalValues(args []value) (value, error) {
	return valuesResult(args), nil
}

func parseCallWithValuesArgs(args []value, name string) (procedure, procedure, error) {
	if len(args) != 2 {
		return nil, nil, newCurrentEvalError("'%s' expects exactly 2 arguments", name)
	}

	producer, ok := args[0].(procedure)
	if !ok {
		return nil, nil, newCurrentEvalError("'%s' expects a procedure for producer, got %s", name, args[0].schemeString())
	}

	consumer, ok := args[1].(procedure)
	if !ok {
		return nil, nil, newCurrentEvalError("'%s' expects a procedure for consumer, got %s", name, args[1].schemeString())
	}

	return producer, consumer, nil
}

func startCallWithValues(m *machine, producer procedure, consumer procedure, pos SourcePos, cont continuation) error {
	return applyProcedureState(m, producer, nil, pos, &callWithValuesProducerFrame{
		consumer: consumer,
		pos:      pos.normalized(),
		next:     cont,
	})
}

func producedValues(v value) []value {
	if values, ok := v.(multiValuesValue); ok {
		items := make([]value, len(values.items))
		copy(items, values.items)
		return items
	}

	return []value{v}
}

func normalizeValueForContinuation(v value, cont continuation) (value, error) {
	values, ok := v.(multiValuesValue)
	if !ok || continuationAcceptsMultipleValues(cont) {
		return v, nil
	}

	switch len(values.items) {
	case 1:
		return values.items[0], nil
	default:
		return nil, newCurrentEvalError("expected 1 value, got %d", len(values.items))
	}
}

func continuationAcceptsMultipleValues(cont continuation) bool {
	if cont == nil {
		return false
	}

	receiver, ok := cont.(multiValueContinuation)
	return ok && receiver.acceptsMultipleValues()
}
