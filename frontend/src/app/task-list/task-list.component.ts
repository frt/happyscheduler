import { Component, OnInit } from '@angular/core';
import { Http } from '@angular/http';

@Component({
    selector: 'app-task-list',
    templateUrl: './task-list.component.html',
    styleUrls: ['./task-list.component.css']
})
export class TaskListComponent implements OnInit {
    tasks = [];

    constructor(private http: Http) {}

    ngOnInit() {
        this.fetchTasks();
    }

    fetchTasks() {
        this.http.get('http://localhost:3000/tasks').subscribe(
            data => this.tasks = JSON.parse(data['_body']).tasks,
            error => alert('An error occurred: ' + error)
        );
    }
}
