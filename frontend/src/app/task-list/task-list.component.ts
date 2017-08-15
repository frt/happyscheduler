import { Component, OnInit } from '@angular/core';
import { Http } from '@angular/http';

@Component({
    selector: 'app-task-list',
    templateUrl: './task-list.component.html',
    styleUrls: ['./task-list.component.css']
})
export class TaskListComponent implements OnInit {
    tasks = [];
    doneTasks = [];

    constructor(private http: Http) {}

    ngOnInit() {
        this.fetchTasks();
    }

    public fetchTasks() {
        this.http.get('../tasks').subscribe(
            data => this.tasks = JSON.parse(data['_body']).tasks,
            error => alert('An error occurred: ' + error)
        );
        this.http.get('../tasks/done').subscribe(
            data => this.doneTasks = JSON.parse(data['_body']).tasks,
            error => alert('An error occurred: ' + error)
        );
    }

    onDeleteClick(id: number) {
        this.http.delete('../tasks/' + id).subscribe(
            () => this.fetchTasks(),
            error => alert('An error occurred: ' + error)
        );
    }

    onDoneClick(id: number) {
        const taskToUpdate = this.tasks.find((task) => { return task.id === id; });
        taskToUpdate.done = true;
        this.http.put('../tasks/' + id, JSON.stringify(taskToUpdate)).subscribe(
            () => this.fetchTasks(),
            error => alert('An error occurred: ' + error)
        );
    }
}
