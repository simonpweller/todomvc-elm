import { Elm } from "../src/Main.elm";

const storedJSON = localStorage.getItem("todos")
const storedTodos = storedJSON ? JSON.parse(storedJSON) : null

const app = Elm.Main.init({
	node: document.querySelector("main"),
	flags: storedTodos
});

app.ports.storeTodos.subscribe(function (todos) {
	localStorage.setItem("todos", JSON.stringify(todos))
})
