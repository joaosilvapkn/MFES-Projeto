sig User {}

sig Meeting {
	var host : lone User,
	var participants : set User
}
var sig active in Meeting {}

pred schedule [m : Meeting, h : User] {
	no m.host
	host' = host + m->h
	participants' = participants
	active' = active
}

pred start [m : Meeting] {
	m not in active
	some m.host
	active' = active + m
	participants' = participants + m->m.host
	host' = host
}

pred join [m : Meeting, u : User] {
	m in active
	u not in m.participants
	participants' = participants + m->u
	host' = host
	active' = active
}

pred leave [m : Meeting, u : User] {
	u in m.participants
	u != m.host
	participants' = participants - m->u
	host' = host
	active' = active
}

pred end [m : Meeting] {
	m in active
	participants' = participants - m->User
	active' = active - m
	host' = host
}

pred stutter {
	participants' = participants
	host' = host
	active' = active
}

fact {
	no host
	no participants
	no active
	always { 
		(some m : Meeting, u : User | schedule[m,u])
		or
		(some m : Meeting | start[m] or end[m])
		or
		(some m : Meeting, u : User | join[m,u] or leave[m,u])
		or
		stutter
	}
}

run {}

check Inv1 {
	always no (Meeting - active).participants
}

check Inv2 {
	always (all m : active | m.host in m.participants)
}
