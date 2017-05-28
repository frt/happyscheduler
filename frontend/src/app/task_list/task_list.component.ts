import { Component } from '@angular/core';
import { Http } from '@angular/http';

@Component({
    selector: 'app-task_list',
    templateUrl: './task_list.component.html',
    styleUrls: ['./task_list.component.css']
})
export class TaskListComponent {
    tasks = [];

    constructor(private http : Http) {}

    ngOnInit() {
        this.fetchTasks();
    }
    
    fetchTasks() {
        this.http.get('http://localhost:3000/tasks').subscribe(
            (data) => {
                this.tasks = JSON.parse(data['_body']).tasks;
            }
        );
    }
}
